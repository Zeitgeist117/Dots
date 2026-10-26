//@ pragma UseQApplication

import QtQuick
import QtQuick.Effects
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Quickshell.WindowManager
import Quickshell.Services.SystemTray
import Quickshell.Services.UPower

Scope {
    id: root

    // ── Shared Theme & State ─────────────────────────────────────────────
    property color colBg:           "#282828"
    property color colFg:           "#ebdbb2"
    property color colDim:          "#928374"
    property color colMute:         "#504945"
    property color colUrgent:       "#fb4934"
    property color colBatActive:    "#B8BB26"
    property color colBatInactive:  "#504945"
    property string fontFamily:     "DepartureMono Nerd Font"
    property int fontSize:          13
    property int gap:               24
    property int rowHeight:         24
    property bool trayTint:         true
    property bool traySilhouette:   false
    property bool kanjiTags:        true
    readonly property var kanji: ["一","二","三","四","五","六","七","八","九"]

    property bool barVisible: true
    property bool forcedByLauncher: false
    property bool trayOpen: false
    property bool launcherOpen: false
    property bool batteryOpen: false

    property int cpuUsage: 0
    property real lastCpuIdle: 0
    property real lastCpuTotal: 0
    property string ramText: "0.0G/0G"

    property var activeTrayItem: null
    property var activeMenuHandle: null
    property var allApps: []

    // ── IPC Handlers ─────────────────────────────────────────────────────
    IpcHandler {
        target: "launcher"
        function toggle(): void {
            root.launcherOpen = !root.launcherOpen
            root.trayOpen = false
            root.batteryOpen = false
        }
    }

    IpcHandler {
        target: "bar"
        function toggle(): void {
            root.forcedByLauncher = false
            root.trayOpen = false
            root.batteryOpen = false
            root.launcherOpen = false
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

    // ── Autocomplete & System Processes ──────────────────────────────────
    readonly property string topSuggestion: {
        const text = cmdInput.text.trim()
        if (text === "") return ""
        const q = text.toLowerCase()
        const match = root.allApps.find(app => app.toLowerCase().startsWith(q))
        return match || ""
    }

    readonly property string ghostText: {
        const text = cmdInput.text
        if (!topSuggestion || text === "") return ""
        if (topSuggestion.toLowerCase().startsWith(text.toLowerCase())) {
            return topSuggestion.substring(text.length)
        }
        return ""
    }

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

    Process {
        id: appScraper
        running: true
        command: ["sh", "-c", "grep -h '^Name=' /usr/share/applications/*.desktop ~/.local/share/applications/*.desktop 2>/dev/null | cut -d= -f2- | sort -u"]
        stdout: SplitParser {
            onRead: data => {
                const name = data.trim()
                if (name && !root.allApps.includes(name)) {
                    root.allApps.push(name)
                }
            }
        }
    }

    Process { id: launcherProc }

    function runLauncherCommand(cmd) {
        if (cmd && cmd.trim() !== "") {
            launcherProc.command = ["sh", "-c", cmd + " &"]
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
            anchors.margins: 0 // Distance from physical screen edge

            Rectangle {
                anchors.fill: parent
                color: "transparent"
                border { width: 5; color: root.colBg } // Substantial border thickness
            }
        }
    }

    // ── WINDOW 2: Top Status Bar & Widgets (Top Layer) ───────────────────
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

        // ── Layout: Left Workspaces ──────────────────────────────────────
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
        Rectangle { width: 1; height: 22; color: root.colDim; x: tagBox.x + tagBox.width + 14; anchors.verticalCenter: parent.verticalCenter }

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
                                text: "             :COMMAND:"
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

                        Text {
                            id: ghostLabel
                            anchors.fill: parent
                            x: textMetrics.width
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
                    color: root.colFg
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
                    }
                }
            }
        }
        Rectangle { width: 1; height: 22; color: root.colDim; anchors { right: parent.right; rightMargin: 14; verticalCenter: parent.verticalCenter } }
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

    // ── Battery Submenu Popup ────────────────────────────────────────────
    PopupWindow {
        id: batteryPopup
        visible: root.batteryOpen && root.hasBattery
        color: "transparent"
        anchor.window: topBarWindow
        anchor.rect.x: rightRow.x + batContainer.x + batContainer.width / 2 - width / 2
        anchor.rect.y: topBarWindow.height + 2
        implicitWidth: 380
        implicitHeight: batMainCol.height + 36
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
                x: 18; y: 18
                width: parent.width - 36
                spacing: 14

                Text {
                    text: "BATTERY :: " + (root.isCharging ? "Charging" : "Discharging")
                    color: root.colFg
                    font.family: root.fontFamily
                    font.pixelSize: root.fontSize
                    font.letterSpacing: 0.5
                    renderType: Text.NativeRendering
                }

                Row {
                    spacing: 16
                    height: 16

                    Row {
                        spacing: 4
                        height: 16
                        Repeater {
                            model: 15
                            Rectangle {
                                required property int index
                                width: 14
                                height: 16
                                color: (index / 15 < (root.battery / 100)) ? root.colBatActive : root.colBatInactive
                            }
                        }
                    }

                    Text {
                        text: root.battery + "%"
                        color: root.colFg
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize + 4
                        font.bold: true
                        height: 16
                        verticalAlignment: Text.AlignVCenter
                        renderType: Text.NativeRendering
                    }
                }

                Column {
                    spacing: 4
                    Text {
                        text: (root.isCharging ? "Time to Full: " : "Time Left: ") + root.formatDuration(root.isCharging ? root.timeToFull : root.timeToEmpty)
                        color: root.colFg
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize - 1
                        renderType: Text.NativeRendering
                    }
                    Text {
                        text: (root.isCharging ? "Charging: +" : "Discharging: ") + root.energyRate.toFixed(1) + "W"
                        color: root.colFg
                        font.family: root.fontFamily
                        font.pixelSize: root.fontSize - 1
                        renderType: Text.NativeRendering
                    }
                }
            }
        }
    }
}
