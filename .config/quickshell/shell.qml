import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import Quickshell.Services.SystemTray
import Quickshell.Io

PanelWindow {
    id: root

    // ── Colors ───────────────────────────────────────────────────────────────
    property color colBg:   "#282828"   // dark bg
    property color colFg:   "#ebdbb2"   // beige/gold text & accents
    property color colGray: "#504945"
    property color colMute: "#928374"

    property string fontFamily: "DepartureMono Nerd Font"
    property int fontSize: 16   // slightly smaller for compact feel

    // CPU & Mem (your existing logic – fill in the Process blocks)
    property int cpuUsage: 0
    property var lastCpuIdle: 0
    property var lastCpuTotal: 0

	Process {
		id: cpuProc
		command: ["sh", "-c", "head -1 /proc/stat"]
		stdout: SplitParser {
			onRead: data => {
				var p = data.trim().split(/\s+/)
				var idle = parseInt(p[4]) + parseInt(p[5])
				var total = p.slice(1, 8).reduce((a, b) => a + parseInt(b), 0)
				if (lastCpuTotal > 0) {
					cpuUsage = Math.round(100 * (1 - (idle - lastCpuIdle) / (total - lastCpuTotal)))
				}
				lastCpuTotal = total
				lastCpuIdle = idle
			}
		}
		Component.onCompleted: running = true
	}
	// memory widget
	property int memUsage: 0
	Process {
		id: memProc
		command: ["sh", "-c", "free | grep Mem"]
		stdout: SplitParser {
			onRead: data => {
				var parts = data.trim().split(/\s+/)
				var total = parseInt(parts[1]) || 1
				var used = parseInt(parts[2]) || 0
				memUsage = Math.round(100 * used / total)
			}
		}
		Component.onCompleted: running = true
	}

    Timer {
        interval: 2000; running: true; repeat: true
        onTriggered: { cpuProc.running = true; memProc.running = true }
    }

    // ── Window setup: floating + transparent bg ──────────────────────────────
    color: "transparent"
    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: 36          // thin but enough for border/shadow space
    exclusiveZone: 36          // reserve space (no overlap with maximized windows)

    // Main floating panel container
    Rectangle {
        id: frame
        anchors {
            fill: parent
            topMargin: 6        // distance from screen top → floating feel
            leftMargin: 7
            rightMargin: 7
        }

        color: root.colBg
        radius: 0              // subtle rounded corners like many EWW bars

        border {
            color: root.colFg
            width: 2          // thin gold/beige outline – matches your screenshots
        }

        // Inner content with padding
        RowLayout {
            anchors.fill: parent
            anchors.margins: 6   // inner padding – creates the framed look
            spacing: 12          // space between major sections

            // ── Left: Japanese kanji workspaces ──────────────────────────────
            Row {
                spacing: 14

                Repeater {
                    model: ["一","二","三","四","五","六","七","八","九"]

                    delegate: Item {
                        width: 14; height: 19

                        Text {
                            anchors.centerIn: parent
                            text: modelData
                            color: {
                                let wsId = index + 1
                                let active = Hyprland.focusedWorkspace?.id === wsId
                                let exists = Hyprland.workspaces.values.some(w => w.id === wsId)
                                return active ? root.colFg : (exists ? root.colMute : root.colGray)
                            }
                            font { family: root.fontFamily; pixelSize: 14 ; bold: true }
                        }

                        // Thin underline only on active

                        MouseArea {
                            anchors.fill: parent
                            onClicked: Hyprland.dispatch("workspace " + (index + 1))
                        }
                    }
                }
            }

            Item { Layout.fillWidth: true }  // push right side away

            // ── Right side: compact icons + separators ───────────────────────
            Row {
                spacing: 10
                layoutDirection: Qt.RightToLeft  // tray last → very right

                // System tray (compact, small icons)
                Row {
                    spacing: 5
                    Repeater {
                        model: SystemTray.items
                        delegate: MouseArea {
                            width: 16; height: 16
                            hoverEnabled: true

                            Image {
                                anchors.centerIn: parent
                                source: modelData.icon
                                sourceSize: Qt.size(24,24)
                                width: 18; height: 18
                                smooth: true
                            }

                            ToolTip.visible: containsMouse
                            ToolTip.text: modelData.tooltipTitle || modelData.title || ""
                            ToolTip.delay: 600

                            onClicked: (mouse) => {
                                if (mouse.button === Qt.LeftButton) modelData.activate()
                                else if (mouse.button === Qt.RightButton && modelData.hasMenu) modelData.display()
                            }
                        }
                    }
                }

                Rectangle { width: 1; height: 16; color: root.colGray }

                // Clock (12h + PM, compact)
                Text {
                    id: clock
                    color: root.colFg
                    font { family: root.fontFamily; pixelSize: root.fontSize; bold: true }

                    Timer {
                        interval: 1000; running: true; repeat: true
                        property date now: new Date()
                        onTriggered: now = new Date()
                        onNowChanged: {
                            let h = now.getHours()
                            let m = now.getMinutes().toString().padStart(2,'0')
                            let ampm = h >= 12 ? "PM" : "AM"
                            h = h % 12 || 12
                            clock.text = h + ":" + m + ampm
                        }
                        triggeredOnStart: true
                    }
                }

                Rectangle { width: 1; height: 16; color: root.colGray }

                Text { text: "󰻠 " + cpuUsage + "%"; color: root.colFg; font { family: root.fontFamily; pixelSize: root.fontSize; bold: true } }
                Rectangle { width: 1; height: 16; color: root.colGray }

                Text { text: " " + memUsage + "%"; color: root.colFg; font { family: root.fontFamily; pixelSize: root.fontSize; bold: true } }

                // If you have battery, add here (example with UPower or Process parse)
                // Rectangle { width: 1; height: 16; color: root.colGray }
                // Text { text: " " + batteryPercent + "%"; ... }
            }
        }
    }
}
