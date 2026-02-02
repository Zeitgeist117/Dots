import Quickshell
import Quickshell.Wayland
import Quickshell.Hyprland
import Quickshell.Io
import QtQuick
import QtQuick.Layouts

PanelWindow {
	id: root
	// declaring colors n shit
	property color colBg: "#282828"
	property color colFg: "#ebdbb2"
	property color colGray: "#504945"
	property color colMute: "#928374"
	property string fontFamily: "Departure Mono"
	property int fontSize: 17
	
	//cpu widget
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
		interval: 2000
		running: true
		repeat: true
		onTriggered: {
			cpuProc.running = true
			memProc.running = true
		}
	}
    anchors.top: true
    anchors.left: true
    anchors.right: true
    implicitHeight: 30
    color: root.colBg
	

    RowLayout {
        anchors.fill: parent
        anchors.margins: 5

        Repeater {
            model: 9

            Text {
                property var ws: Hyprland.workspaces.values.find(w => w.id === index + 1)
                property bool isActive: Hyprland.focusedWorkspace?.id === index + 1

                text: index + 1
                color: isActive
                    ? root.colFg 
                    : (ws ? root.colMute : root.colGray)

				font { family: root.fontFamily; pixelSize: root.fontSize; bold: true}

                MouseArea {
                    anchors.fill: parent
                    onClicked: Hyprland.dispatch("workspace " + (index + 1))
                }
            }
        }

        Item { Layout.fillWidth: true }
		Rectangle { width: 1; height: 16; color: root.colGray }
		Text {
			text: "󰻠 " + cpuUsage + "%"
			color: root.colFg
			font { family: root.fontFamily; pixelSize: root.fontSize; bold: true}
		}
		
		Rectangle { width: 1; height: 16; color: root.colGray }

		Text {
			text: " " + memUsage + "%"
			color: root.colFg
			font { family: root.fontFamily; pixelSize: root.fontSize; bold: true}
		}
		Rectangle { width: 1; height: 16; color: root.colBg }
    }
}
