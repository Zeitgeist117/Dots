
//@ pragma UseQApplication

import QtQuick
import QtQuick.Effects
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.WindowManager
import Quickshell.Services.SystemTray
import Quickshell.Services.UPower
import Quickshell.Services.Mpris

Scope {
    id: root

    // ── Shared Theme & State ─────────────────────────────────────────────
    property color colBg:             "#282828"
    property color colFg:             "#ebdbb2"
    property color colDim:            "#928374"
    property color colMute:           "#504945"
    property color colUrgent:         "#fb4934"
    property color colBatActive:      "#B8BB26"
    property color colBatInactive:    "#504945"
    property string fontFamily:       "CozzetteVector Mono"
    property int fontSize:            13
    property int gap:                 24
    property int rowHeight:           24
	property int musicMarqueeThreshold:  55
    property bool trayTint:           true
    property bool traySilhouette:     false
    property bool kanjiTags:          true
    readonly property var kanji: ["一","二","三","四","五","六","七","八","九"]

    property bool barVisible: true
    property bool forcedByLauncher: false
    property bool trayOpen: false
    property bool launcherOpen: false
    property bool batteryOpen: false
    property bool musicOpen: false

    property int cpuUsage: 0
    property real lastCpuIdle: 0
    property real lastCpuTotal: 0
    property string ramText: "0.0G/0G"

    property var activeTrayItem: null
    property var activeMenuHandle: null

    // Every executable on $PATH (like `rofi -show run`). Assigned as a whole
    // so the autocomplete bindings update.
    property var allApps: []

    // Visualizer: 5 levels (0-9). Fed by `cava` when it is installed, otherwise by a
    // random fallback, so the bars never depend on declarative animations.
    property var vizLevels: [0, 0, 0, 0, 0]
    property string cavaMethod: "pipewire"   // or "pulse" / "alsa" -- must match your audio server
    property bool vizFallback: false          // true = random bars when cava gives no signal
    property bool cavaWorking: false     // true once cava has delivered a non-zero frame
    readonly property bool musicPlaying: leftMusicContainer.player ? leftMusicContainer.player.isPlaying : false
    onMusicPlayingChanged: if (!musicPlaying) cavaWorking = false

    // ── IPC Handlers ─────────────────────────────────────────────────────
    IpcHandler {
        target: "launcher"
        function toggle(): void {
            root.launcherOpen = !root.launcherOpen
            root.trayOpen = false
            root.batteryOpen = false
            root.musicOpen = false
        }
    }

    IpcHandler {
        target: "bar"
        function toggle(): void {
            root.forcedByLauncher = false
            root.trayOpen = false
            root.batteryOpen = false
            root.launcherOpen = false
            root.musicOpen = false
            root.barVisible = !root.barVisible
        }
    }

    onLauncherOpenChanged: {
        if (launcherOpen && !barVisible) {
            barVisible = true
            forcedByLauncher = true
        } else if (!launcherOpen && forcedByLauncher) {
            barVisible = false
            forcedByLauncher = false
        }
    }

    // ── Run-launcher autocomplete ────────────────────────────────────────
    // Completes only the first word, so arguments are left alone.
    // Case-sensitive (commands are) and prefers the shortest match.
    readonly property string topSuggestion: {
        const text = cmdInput.text
        if (text === "" || /\s/.test(text)) return ""
        let best = ""
        for (const c of root.allApps)
            if (c.startsWith(text) && c !== text && (best === "" || c.length < best.length))
                best = c
        return best
    }

    readonly property string ghostText: topSuggestion ? topSuggestion.substring(cmdInput.text.length) : ""

    readonly property var batDevice: UPower.displayDevice
    readonly property bool hasBattery: batDevice ? batDevice.isPresent : false
    readonly property bool isCharging: batDevice ? (batDevice.state === UPowerDeviceState.Charging) : false
    readonly property real batRaw: batDevice ? batDevice.percentage : 0
    readonly property int battery: Math.round(batRaw <= 1 ? batRaw * 100 : batRaw)
    readonly property real energyRate: batDevice ? batDevice.energyRate : 0
    readonly property real timeToEmpty: batDevice ? batDevice.timeToEmpty : 0
    readonly property real timeToFull: batDevice ? batDevice.timeToFull : 0

    function formatDuration(seconds) {
        if (!seconds || seconds <= 0) return "Calculating..."
        const h = Math.floor(seconds / 3600)
        const m = Math.floor((seconds % 3600) / 60)
        if (h > 0) return h + " Hours " + m + " Minutes"
        return m + " Minutes"
    }

    function formatTime(sec) {
        if (!sec || sec <= 0 || isNaN(sec)) return "00:00"
        const m = Math.floor(sec / 60)
        const s = Math.floor(sec % 60)
        return (m < 10 ? "0" + m : m) + ":" + (s < 10 ? "0" + s : s)
    }

    // Scan every directory on $PATH (following symlinks) for executables.
    Process {
        id: appScraper
        running: true
        command: ["bash", "-c",
            "IFS=:; find -L $PATH -maxdepth 1 -type f -executable -printf '%f\\n' 2>/dev/null | sort -u"]
        stdout: StdioCollector {
            onStreamFinished: root.allApps = text.split("\n").filter(c => c !== "")
        }
    }

    Process { id: launcherProc }

    // Real audio levels from cava, only while something is playing.
    Process {
        id: cavaProc
        running: root.musicPlaying
        command: ["sh", "-c", [
            "command -v cava >/dev/null 2>&1 || exit 127",
            "cat > /tmp/qs-cava.conf <<'EOF'",
            "[general]",
            "bars = 5",
            "framerate = 30",
            "[input]",
            "method = " + root.cavaMethod,
            "source = auto",
            "[smoothing]",
            "noise_reduction = 60",
            "[output]",
            "method = raw",
            "channels = mono",
            "mono_option = average",
            "raw_target = /dev/stdout",
            "data_format = ascii",
            "ascii_max_range = 9",
            "bar_delimiter = 59",
            "frame_delimiter = 10",
            "EOF",
            "exec cava -p /tmp/qs-cava.conf"
        ].join("\n")]
        stdout: SplitParser {
            onRead: data => {
                const v = data.split(";").filter(x => x !== "").map(Number)
                if (v.some(x => x > 0)) {
                    if (!root.cavaWorking) console.log("[viz] using cava")
                    root.cavaWorking = true
                    root.vizLevels = v
                } else if (root.cavaWorking) {
                    root.vizLevels = v     // real silence once cava is known to work
                }
            }
        }
        stderr: StdioCollector {
            onStreamFinished: if (text.trim() !== "") console.log("[viz] cava says: " + text.trim())
        }
        onExited: (code, status) => console.log("[viz] cava exited, code " + code + (code === 127 ? " (not installed)" : ""))
    }

    // Fallback while cava is missing, silent or not working: lively random levels
    Timer {
        interval: 130
        repeat: true
        running: root.vizFallback && root.musicPlaying && !root.cavaWorking
        onTriggered: root.vizLevels = Array.from({ length: 5 }, () => Math.floor(Math.random() * 10))
    }

    // Hand the typed text to the shell, detached (setsid) so the program
    // outlives the bar. The text is passed as a quoted argument, so spaces,
    // pipes, quotes and arguments behave as they would in a terminal.
    function runLauncherCommand(cmd) {
        const c = cmd.trim()
        if (c !== "") {
            launcherProc.running = false
            launcherProc.command = ["sh", "-c",
                "setsid -f sh -c \"$1\" >/dev/null 2>&1 </dev/null", "sh", c]
            launcherProc.running = true
        }
        root.launcherOpen = false
        cmdInput.text = ""
    }

    SystemClock { id: clock; precision: SystemClock.Minutes }

    Process {
        id: cpuProc
        command: ["sh", "-c", "head -1 /proc/stat"]
        stdout: SplitParser {
            onRead: data => {
                const p = data.trim().split(/\s+/)
                const idle = parseInt(p[4]) + parseInt(p[5])
                const total = p.slice(1, 8).reduce((a, b) => a + parseInt(b), 0)
                if (root.lastCpuTotal > 0)
                    root.cpuUsage = Math.round(100 * (1 - (idle - root.lastCpuIdle) / (total - root.lastCpuTotal)))
                root.lastCpuTotal = total
                root.lastCpuIdle = idle
            }
        }
    }

    Process {
        id: memProc
        command: ["sh", "-c", "free -m | grep Mem"]
        stdout: SplitParser {
            onRead: data => {
                const p = data.trim().split(/\s+/)
                const totalM = parseInt(p[1]) || 1
                const usedM = parseInt(p[2]) || 0
                const usedG = (usedM / 1024).toFixed(1)
                const totalG = Math.round(totalM / 1024)
                root.ramText = usedG + "G/" + totalG + "G"
            }
        }
    }

    Timer {
        interval: 2000; running: true; repeat: true; triggeredOnStart: true
        onTriggered: { cpuProc.running = true; memProc.running = true }
    }

    // ── WINDOW 1: Full-Screen Substantial Border Frame (Background Layer) ──
    PanelWindow {
        anchors { top: true; bottom: true; left: true; right: true }
        color: "transparent"
        visible: root.barVisible
        WlrLayershell.layer: WlrLayer.Background
        WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

        Item {
            anchors.fill: parent
            anchors.margins: 0

            Rectangle {
                anchors.fill: parent
                color: "transparent"
                border { width: 5; color: root.colBg }
            }
        }
    }

    // ── WINDOW 2: Top Status Bar (Top Layer) ─────────────────────────────
    PanelWindow {
        id: topBarWindow
        anchors { top: true; left: true; right: true }
        implicitHeight: 31
        color: root.colBg
        visible: root.barVisible

        WlrLayershell.layer: WlrLayer.Top
        WlrLayershell.keyboardFocus: root.launcherOpen
            ? WlrKeyboardFocus.Exclusive
            : WlrKeyboardFocus.None

        // ── Layout: Left Workspaces & SubTUI ─────────────────────────
        Rectangle { width: 1; height: 22; color: root.colDim; x: 14; anchors.verticalCenter: parent.verticalCenter }
        Rectangle {
            id: tagBox
            x: 28
            anchors.verticalCenter: parent.verticalCenter
            width: tagRow.width + 24
            height: 24
            color: "transparent"
            border { width: 1; color: root.colDim }

            Row {
                id: tagRow
                anchors.centerIn: parent
                height: parent.height

                Repeater {
                    model: WindowManager.windowsets.filter(w => w.shouldDisplay)
                    delegate: Item {
                        required property var modelData
                        readonly property var set: modelData
                        readonly property bool active: set ? set.active : false
                        readonly property string label: {
                            const n = parseInt(set.name)
                            return (root.kanjiTags && n >= 1 && n <= 9) ? root.kanji[n - 1] : set.name
                        }
                        implicitWidth: 38
                        height: tagBox.height

                        Text {
                            anchors.fill: parent
                            text: parent.active ? "[ " + parent.label + " ]" : parent.label
                            color: parent.active ? root.colFg : (parent.set.urgent ? root.colUrgent : root.colDim)
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            font.letterSpacing: 0.5
                            verticalAlignment: Text.AlignVCenter
                            horizontalAlignment: Text.AlignHCenter
                            renderType: Text.NativeRendering
                        }
                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: if (parent.set.canActivate) parent.set.activate()
                        }
                    }
                }
            }
        }
        Rectangle { id: leftSep2; width: 1; height: 22; color: root.colDim; x: tagBox.x + tagBox.width + 14; anchors.verticalCenter: parent.verticalCenter }

		// ── SubTUI Music Widget (Ignores Web Browsers / YouTube) ───────────
		Item {
			id: leftMusicContainer
			readonly property var player: {
				const players = Mpris.players.values
				if (!players || players.length === 0) return null

				const ignoreKeywords = ["firefox", "chrome", "chromium", "brave", "edge", "opera", "browser", "plasma-browser-integration"]

				const preferred = players.find(p => {
					const id = (p.identity || "").toLowerCase()
					return id.includes("subtui") || id.includes("mpv") || id.includes("spotify") || id.includes("vlc")
				})
				if (preferred) return preferred

				const nonBrowserPlayers = players.filter(p => {
					const id = (p.identity || "").toLowerCase()
					const entry = (p.desktopEntry || "").toLowerCase()
					return !ignoreKeywords.some(kw => id.includes(kw) || entry.includes(kw))
				})

				return nonBrowserPlayers.length > 0 ? nonBrowserPlayers[0] : null
			}
			visible: player !== null
			x: leftSep2.x + 14
			anchors.verticalCenter: parent.verticalCenter

			width: visible ? (marqueeClip.isLongText ? 240 : Math.max(120, musicTextStatic.implicitWidth + (vizRow.visible ? vizRow.width + 16 : 0) + 24)) : 0
			height: 24

			Behavior on width { NumberAnimation { duration: 150 } }

			Rectangle {
				anchors.fill: parent
				color: "transparent"
				border { width: 1; color: root.musicOpen ? root.colFg : root.colDim }

				Row {
					anchors.fill: parent
					anchors.leftMargin: 8
					anchors.rightMargin: 8
					spacing: 6

					Row {
						id: vizRow
						spacing: 2
						height: 12
						anchors.verticalCenter: parent.verticalCenter
						visible: leftMusicContainer.player ? leftMusicContainer.player.isPlaying : false

						Repeater {
							model: 5
							Rectangle {
								required property int index
								width: 2
								height: 3 + Math.max(0, Math.min(9, root.vizLevels[index] || 0))
								color: root.colBatActive
								anchors.verticalCenter: parent.verticalCenter

								Behavior on height { NumberAnimation { duration: 90 } }
							}
						}
					}

					Item {
						id: marqueeClip
						height: parent.height
						anchors.verticalCenter: parent.verticalCenter
						width: leftMusicContainer.width - (vizRow.visible ? vizRow.width + 6 : 0) - 16
						clip: true

						readonly property string trackString: {
							const p = leftMusicContainer.player
							if (!p) return "IDLE"
							const artist = p.trackArtist || (p.metadata && p.metadata["xesam:artist"] ? p.metadata["xesam:artist"].toString() : "")
							const title = p.trackTitle || (p.metadata && p.metadata["xesam:title"] ? p.metadata["xesam:title"].toString() : (p.identity || "ACTIVE"))
							const content = artist ? (artist + " - " + title) : title
							return content.toUpperCase()
						}

						// Uses the root variable now
						readonly property bool isLongText: trackString.length > root.musicMarqueeThreshold

						Text {
							id: musicTextStatic
							anchors.verticalCenter: parent.verticalCenter
							text: marqueeClip.trackString
							color: root.colFg
							font.family: root.fontFamily
							font.pixelSize: root.fontSize
							font.letterSpacing: 0.5
							renderType: Text.NativeRendering
							visible: !marqueeClip.isLongText
						}

						Text {
							id: musicTextScroll
							anchors.verticalCenter: parent.verticalCenter
							text: marqueeClip.trackString
							color: root.colFg
							font.family: root.fontFamily
							font.pixelSize: root.fontSize
							font.letterSpacing: 0.5
							renderType: Text.NativeRendering
							visible: marqueeClip.isLongText

							x: marqueeAnim.running ? marqueeClip.width : 0

							NumberAnimation {
								id: marqueeAnim
								target: musicTextScroll
								property: "x"
								from: marqueeClip.width
								to: -musicTextScroll.implicitWidth
								duration: Math.max(6000, musicTextScroll.implicitWidth * 30)
								loops: Animation.Infinite
								running: marqueeClip.isLongText && musicTextScroll.implicitWidth > marqueeClip.width
							}
						}
					}
				}
			}

			MouseArea {
				anchors.fill: parent
				cursorShape: Qt.PointingHandCursor
				acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
				onClicked: mouse => {
					const p = leftMusicContainer.player
					if (!p) return
					if (mouse.button === Qt.LeftButton) {
						root.musicOpen = !root.musicOpen
						root.trayOpen = false
						root.batteryOpen = false
					} else if (mouse.button === Qt.RightButton) {
						p.next()
					} else if (mouse.button === Qt.MiddleButton) {
						p.previous()
					}
				}
			}
		}

        Rectangle {
            id: leftSep3
            width: 1
            height: 22
            color: root.colDim
            x: leftMusicContainer.visible ? leftMusicContainer.x + leftMusicContainer.width + 14 : leftSep2.x
            visible: leftMusicContainer.visible
            anchors.verticalCenter: parent.verticalCenter
        }

        // ── Center Bar Container (Bordered Title / Animated Launcher) ────
        Item {
            id: centerContainer
            anchors.centerIn: parent
            width: root.launcherOpen ? 300 : titleLabel.implicitWidth + 24
            height: 24

            Behavior on width {
                NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
            }

            Rectangle {
                id: titleBox
                opacity: root.launcherOpen ? 0 : 1
                visible: opacity > 0
                anchors.fill: parent
                color: "transparent"
                border { width: 1; color: root.colDim }

                Behavior on opacity { NumberAnimation { duration: 100 } }

                Text {
                    id: titleLabel
                    anchors.centerIn: parent
                    text: root.trayOpen ? "[ ZEITGEIST117 ]" : "ZEITGEIST117"
                    color: root.colFg
                    font.family: root.fontFamily
                    font.pixelSize: root.fontSize
                    font.letterSpacing: 0.5
                    verticalAlignment: Text.AlignVCenter
                    horizontalAlignment: Text.AlignHCenter
                    renderType: Text.NativeRendering
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    acceptedButtons: Qt.LeftButton | Qt.RightButton
                    onClicked: {
                        root.trayOpen = !root.trayOpen
                        root.batteryOpen = false
                        root.musicOpen = false
                    }
                }
            }

            Rectangle {
                id: inlineLauncher
                opacity: root.launcherOpen ? 1 : 0
                visible: opacity > 0
                anchors.fill: parent
                color: "transparent"
                border { width: 1; color: root.colDim }
                clip: true

                Behavior on opacity { NumberAnimation { duration: 120 } }

                onVisibleChanged: {
                    if (visible) {
                        Qt.callLater(() => cmdInput.forceActiveFocus())
                    }
                }

                Row {
                    anchors.centerIn: parent
                    height: parent.height
                    spacing: 6

                    Text {
                        text: "["
                        color: root.colDim
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize
                        height: parent.height
                        verticalAlignment: Text.AlignVCenter
                        renderType: Text.NativeRendering
                    }

                    Item {
                        width: 220
                        height: parent.height
                        anchors.verticalCenter: parent.verticalCenter

                        TextInput {
                            id: cmdInput
                            anchors.fill: parent
                            verticalAlignment: TextInput.AlignVCenter
                            horizontalAlignment: TextInput.AlignLeft
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            renderType: Text.NativeRendering
                            clip: true
                            focus: true

                            Text {
                                text: "            :COMMAND:"
                                color: root.colMute
                                font.family: root.fontFamily
                                font.pixelSize: root.fontSize
                                visible: !cmdInput.text && !cmdInput.inputMethodComposing
                                anchors.fill: parent
                                verticalAlignment: Text.AlignVCenter
                                horizontalAlignment: Text.AlignLeft
                                renderType: Text.NativeRendering
                            }

                            Keys.onTabPressed: {
                                if (root.topSuggestion) {
                                    cmdInput.text = root.topSuggestion
                                    cmdInput.cursorPosition = cmdInput.text.length
                                }
                            }
                            Keys.onReturnPressed: root.runLauncherCommand(cmdInput.text)
                            Keys.onEnterPressed: root.runLauncherCommand(cmdInput.text)
                            Keys.onEscapePressed: {
                                root.launcherOpen = false
                                cmdInput.text = ""
                            }
                        }

                        // Ghost-text autocomplete: starts where the typed text ends
                        // (anchors.fill used to override x and drew it over the input)
                        Text {
                            id: ghostLabel
                            x: textMetrics.advanceWidth
                            width: parent.width - x
                            height: parent.height
                            text: root.ghostText
                            color: root.colMute
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            verticalAlignment: Text.AlignVCenter
                            renderType: Text.NativeRendering
                            visible: root.ghostText !== ""
                        }

                        TextMetrics {
                            id: textMetrics
                            font: cmdInput.font
                            text: cmdInput.text
                        }
                    }

                    Text {
                        text: "      ]"
                        color: root.colDim
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize
                        height: parent.height
                        verticalAlignment: Text.AlignVCenter
                        renderType: Text.NativeRendering
                    }
                }
            }
        }

        // ── Right Side Status Modules ────────────────────────────────────
        Rectangle { width: 1; height: 22; color: root.colDim; x: rightRow.x - 14; anchors.verticalCenter: parent.verticalCenter }
        Row {
            id: rightRow
            anchors { right: parent.right; rightMargin: 30; verticalCenter: parent.verticalCenter }
            spacing: root.gap

            Text {
                text: "[CPU: " + root.cpuUsage + "%]"
                color: root.colFg
                font.family: root.fontFamily
                font.pixelSize: root.fontSize
                font.letterSpacing: 0.5
                height: root.rowHeight
                verticalAlignment: Text.AlignVCenter
                renderType: Text.NativeRendering
            }

            Text {
                text: "[RAM: " + root.ramText + "]"
                color: root.colFg
                font.family: root.fontFamily
                font.pixelSize: root.fontSize
                font.letterSpacing: 0.5
                height: root.rowHeight
                verticalAlignment: Text.AlignVCenter
                renderType: Text.NativeRendering
            }

            Text {
                text: "[CAL: " + Qt.formatDateTime(clock.date, "ddd MMM dd") + "]"
                color: root.colFg
                font.family: root.fontFamily
                font.pixelSize: root.fontSize
                font.letterSpacing: 0.5
                height: root.rowHeight
                verticalAlignment: Text.AlignVCenter
                renderType: Text.NativeRendering
            }

            Text {
                text: "[TIME: " + Qt.formatDateTime(clock.date, "hh:mm AP") + "]"
                color: root.colFg
                font.family: root.fontFamily
                font.pixelSize: root.fontSize
                font.letterSpacing: 0.5
                height: root.rowHeight
                verticalAlignment: Text.AlignVCenter
                renderType: Text.NativeRendering
            }

            Item {
                id: batContainer
                visible: root.hasBattery
                implicitWidth: batText.implicitWidth
                height: root.rowHeight
                anchors.verticalCenter: parent.verticalCenter

                Text {
                    id: batText
                    anchors.fill: parent
                    text: root.isCharging ? "[CHARGING: " + root.battery + "%]" : "[BATTERY: " + root.battery + "%]"
                    color: root.batteryOpen ? root.colFg : (root.isCharging ? root.colBatActive : root.colFg)
                    font.family: root.fontFamily
                    font.pixelSize: root.fontSize
                    font.letterSpacing: 0.5
                    verticalAlignment: Text.AlignVCenter
                    renderType: Text.NativeRendering
                }

                MouseArea {
                    anchors.fill: parent
                    cursorShape: Qt.PointingHandCursor
                    onClicked: {
                        root.batteryOpen = !root.batteryOpen
                        root.trayOpen = false
                        root.musicOpen = false
                    }
                }
            }
        }
        Rectangle { width: 1; height: 22; color: root.colDim; anchors { right: parent.right; rightMargin: 14; verticalCenter: parent.verticalCenter } }
    }

    // ── Music Player Popup (Album Art, Progress Bar, Controls) ────────────
    PopupWindow {
        id: musicPopup
        visible: root.musicOpen && leftMusicContainer.player !== null
        color: "transparent"
        anchor.window: topBarWindow
        anchor.rect.x: leftMusicContainer.x
        anchor.rect.y: topBarWindow.height + 2
        implicitWidth: 320
        implicitHeight: musicMainCol.height + 28
        grabFocus: false

        onVisibleChanged: {
            if (!visible && root.musicOpen) {
                root.musicOpen = false
            }
        }

        Rectangle {
            anchors.fill: parent
            color: root.colBg
            border { width: 1; color: root.colDim }

            Column {
                id: musicMainCol
                x: 14; y: 14
                width: parent.width - 28
                spacing: 12

                Row {
                    width: parent.width
                    Text {
                        text: "[PLAYER :: " + (leftMusicContainer.player ? leftMusicContainer.player.identity.toUpperCase() : "SUBTUI") + "]"
                        color: root.colDim
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize - 1
                        font.letterSpacing: 0.5
                        renderType: Text.NativeRendering
                    }
                }

                Row {
                    width: parent.width
                    spacing: 14

                    Rectangle {
                        width: 70; height: 70
                        color: root.colMute
                        border { width: 1; color: root.colDim }

                        Image {
                            id: albumArtImg
                            anchors.fill: parent
                            anchors.margins: 1
                            source: {
                                const p = leftMusicContainer.player
                                if (!p) return ""
                                if (p.artUrl && p.artUrl !== "") return p.artUrl
                                if (p.metadata && p.metadata["mpris:artUrl"]) return p.metadata["mpris:artUrl"].toString()
                                if (p.metadata && p.metadata["xesam:artUrl"]) return p.metadata["xesam:artUrl"].toString()
                                return ""
                            }
                            fillMode: Image.PreserveAspectCrop
                            asynchronous: true
                            cache: false
                        }

                        Text {
                            anchors.centerIn: parent
                            text: "[ART]"
                            color: root.colDim
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize - 2
                            visible: albumArtImg.status !== Image.Ready
                        }
                    }

                    Column {
                        width: parent.width - 84
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: 4

                        Text {
                            width: parent.width
                            text: leftMusicContainer.player && leftMusicContainer.player.trackTitle ? leftMusicContainer.player.trackTitle.toUpperCase() : "NO TRACK"
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            font.bold: true
                            elide: Text.ElideRight
                            renderType: Text.NativeRendering
                        }

                        Text {
                            width: parent.width
                            text: leftMusicContainer.player && leftMusicContainer.player.trackArtist ? leftMusicContainer.player.trackArtist.toUpperCase() : "UNKNOWN ARTIST"
                            color: root.colDim
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize - 1
                            elide: Text.ElideRight
                            renderType: Text.NativeRendering
                        }

                        Text {
                            width: parent.width
                            text: leftMusicContainer.player && leftMusicContainer.player.trackAlbum ? leftMusicContainer.player.trackAlbum.toUpperCase() : ""
                            color: root.colMute
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize - 2
                            elide: Text.ElideRight
                            visible: text !== ""
                            renderType: Text.NativeRendering
                        }
                    }
                }

                Column {
                    width: parent.width
                    spacing: 4

                    Rectangle {
                        id: progressBarBg
                        width: parent.width
                        height: 8
                        color: root.colMute
                        border { width: 1; color: root.colDim }

                        Rectangle {
                            height: parent.height - 2
                            x: 1; y: 1
                            width: {
                                const p = leftMusicContainer.player
                                if (!p || !p.length || p.length <= 0) return 0
                                return Math.min(parent.width - 2, Math.max(0, (parent.width - 2) * (p.position / p.length)))
                            }
                            color: root.colFg
                        }

                        MouseArea {
                            anchors.fill: parent
                            cursorShape: Qt.PointingHandCursor
                            onClicked: mouse => {
                                const p = leftMusicContainer.player
                                if (p && p.length > 0) {
                                    const targetPos = (mouse.x / width) * p.length
                                    p.position = targetPos
                                }
                            }
                        }
                    }

					Row {
						width: parent.width
						spacing: 8  // <── Adds a clean gap between elapsed and total duration

						Text {
							text: formatTime(leftMusicContainer.player ? leftMusicContainer.player.position : 0)
							color: root.colDim
							font.family: root.fontFamily
							font.pixelSize: root.fontSize - 1
							renderType: Text.NativeRendering
						}

						Text {
							text: "/"
							color: root.colDim
							font.family: root.fontFamily
							font.pixelSize: root.fontSize - 1
							renderType: Text.NativeRendering
						}

						Text {
							text: formatTime(leftMusicContainer.player ? leftMusicContainer.player.length : 0)
							color: root.colDim
							font.family: root.fontFamily
							font.pixelSize: root.fontSize - 1
							renderType: Text.NativeRendering
						}
					}
                }

                Row {
                    anchors.horizontalCenter: parent.horizontalCenter
                    spacing: 16

                    Rectangle {
                        width: 50; height: 24
                        color: "transparent"
                        border { width: 1; color: prevBtnArea.containsMouse ? root.colFg : root.colDim }

                        Text {
                            anchors.centerIn: parent
                            text: "<<"
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            renderType: Text.NativeRendering
                        }

                        MouseArea {
                            id: prevBtnArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: if (leftMusicContainer.player) leftMusicContainer.player.previous()
                        }
                    }

                    Rectangle {
                        width: 70; height: 24
                        color: "transparent"
                        border { width: 1; color: playBtnArea.containsMouse ? root.colFg : root.colDim }

                        Text {
                            anchors.centerIn: parent
                            text: (leftMusicContainer.player && leftMusicContainer.player.isPlaying) ? "[PAUSE]" : "[PLAY]"
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            renderType: Text.NativeRendering
                        }

                        MouseArea {
                            id: playBtnArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: {
                                const p = leftMusicContainer.player
                                if (p) {
                                    if (p.isPlaying) {
                                        p.pause()
                                    } else {
                                        p.play()
                                    }
                                }
                            }
                        }
                    }

                    Rectangle {
                        width: 50; height: 24
                        color: "transparent"
                        border { width: 1; color: nextBtnArea.containsMouse ? root.colFg : root.colDim }

                        Text {
                            anchors.centerIn: parent
                            text: ">>"
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            renderType: Text.NativeRendering
                        }

                        MouseArea {
                            id: nextBtnArea
                            anchors.fill: parent
                            hoverEnabled: true
                            cursorShape: Qt.PointingHandCursor
                            onClicked: if (leftMusicContainer.player) leftMusicContainer.player.next()
                        }
                    }
                }
            }
        }
    }

    // ── Battery Submenu Popup (Floating / Non-Reserving) ─────────────────
    PopupWindow {
        id: batteryPopup
        visible: root.batteryOpen && root.hasBattery
        color: "transparent"
        anchor.window: topBarWindow
        anchor.rect.x: topBarWindow.width - width - 14
        anchor.rect.y: topBarWindow.height + 2
        implicitWidth: 320
        implicitHeight: batMainCol.height + 28
        grabFocus: false

        onVisibleChanged: {
            if (!visible && root.batteryOpen) {
                root.batteryOpen = false
            }
        }

        Rectangle {
            anchors.fill: parent
            color: root.colBg
            border { width: 1; color: root.colDim }

            Column {
                id: batMainCol
                x: 14; y: 14
                width: parent.width - 28
                spacing: 10

                Text {
                    text: root.isCharging ? "[STATUS :: CHARGING]" : "[STATUS :: DISCHARGING]"
                    color: root.colFg
                    font.family: root.fontFamily
                    font.pixelSize: root.fontSize
                    font.letterSpacing: 0.5
                    renderType: Text.NativeRendering
                }

                Row {
                    spacing: 12
                    height: 16

                    Row {
                        spacing: 3
                        height: 16
                        Repeater {
                            model: 12
                            Rectangle {
                                required property int index
                                width: 14
                                height: 16
                                color: (index / 12 < (root.battery / 100)) ? root.colBatActive : root.colBatInactive
                            }
                        }
                    }

                    Text {
                        text: "[" + root.battery + "%]"
                        color: root.colFg
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize
                        font.bold: true
                        height: 16
                        verticalAlignment: Text.AlignVCenter
                        renderType: Text.NativeRendering
                    }
                }

                Rectangle {
                    width: parent.width
                    height: 1
                    color: root.colMute
                }

                Column {
                    width: parent.width
                    spacing: 4
                    Text {
                        text: "[TIME: " + root.formatDuration(root.isCharging ? root.timeToFull : root.timeToEmpty).toUpperCase() + "]"
                        color: root.colDim
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize - 1
                        font.letterSpacing: 0.5
                        renderType: Text.NativeRendering
                    }
                    Text {
                        text: "[RATE: " + root.energyRate.toFixed(1) + "W]"
                        color: root.colDim
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize - 1
                        font.letterSpacing: 0.5
                        renderType: Text.NativeRendering
                    }
                }
            }
        }
    }

    // ── System Tray Popup ────────────────────────────────────────────────
    PopupWindow {
        id: trayPopup
        visible: root.trayOpen
        color: "transparent"
        anchor.window: topBarWindow
        anchor.rect.x: centerContainer.x + centerContainer.width / 2 - width / 2
        anchor.rect.y: topBarWindow.height + 2
        implicitWidth: 260
        implicitHeight: Math.min(400, trayCol.height + 16)
        grabFocus: true

        onVisibleChanged: {
            if (!visible && root.trayOpen) {
                root.trayOpen = false
            }
        }

        Rectangle {
            anchors.fill: parent
            color: root.colBg
            border { width: 1; color: root.colDim }

            Column {
                id: trayCol
                x: 8; y: 8
                width: parent.width - 16

                Repeater {
                    model: SystemTray.items
                    delegate: MouseArea {
                        id: trayItem
                        required property var modelData
                        width: trayCol.width
                        height: root.rowHeight
                        hoverEnabled: true
                        cursorShape: Qt.PointingHandCursor
                        acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton
                        Rectangle {
                            anchors.fill: parent
                            color: trayItem.containsMouse ? root.colMute : "transparent"
                        }
                        Image {
                            id: iconImg
                            x: 6
                            anchors.verticalCenter: parent.verticalCenter
                            width: 16; height: 16
                            source: trayItem.modelData.icon
                            sourceSize: Qt.size(32, 32)
                            smooth: true
                            visible: !root.trayTint
                        }
                        MultiEffect {
                            anchors.fill: iconImg
                            source: iconImg
                            visible: root.trayTint
                            saturation: -1.0
                            brightness: root.traySilhouette ? 1.0 : 0.25
                            colorization: 1.0
                            colorizationColor: root.colFg
                        }
                        Text {
                            anchors { left: iconImg.right; leftMargin: 10; right: parent.right; rightMargin: 6 }
                            text: (trayItem.modelData.tooltipTitle || trayItem.modelData.title || trayItem.modelData.id).toUpperCase()
                            color: root.colFg
                            font.family: root.fontFamily
                            font.pixelSize: root.fontSize
                            font.letterSpacing: 0.5
                            height: root.rowHeight
                            verticalAlignment: Text.AlignVCenter
                            renderType: Text.NativeRendering
                            elide: Text.ElideRight
                        }
                        onClicked: mouse => {
                            if (mouse.button === Qt.MiddleButton) {
                                modelData.secondaryActivate()
                            } else if (mouse.button === Qt.RightButton || (modelData.onlyMenu && modelData.hasMenu)) {
                                if (modelData.hasMenu && modelData.menu) {
                                    root.activeMenuHandle = modelData.menu
                                    root.activeTrayItem = modelData
                                    root.trayOpen = false
                                    Qt.callLater(() => {
                                        nativeMenuAnchor.open()
                                    })
                                }
                            } else {
                                modelData.activate()
                                root.trayOpen = false
                            }
                        }
                    }
                }
            }
        }
    }

    QsMenuAnchor {
        id: nativeMenuAnchor
        menu: root.activeMenuHandle
        anchor.window: topBarWindow
        anchor.rect.x: centerContainer.x + centerContainer.width / 2 - 130
        anchor.rect.y: topBarWindow.height + 2

        onClosed: {
            root.activeTrayItem = null
            root.activeMenuHandle = null
        }
    }
}
