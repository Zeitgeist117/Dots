import QtQuick
import Quickshell
import Quickshell.Wayland
import Quickshell.WindowManager

PanelWindow {
    id: topBarWindow
    anchors { top: true; left: true; right: true }
    implicitHeight: 31
    color: colBg
    visible: barVisible

    WlrLayershell.layer: WlrLayer.Top
    WlrLayershell.keyboardFocus: launcherOpen 
        ? WlrKeyboardFocus.Exclusive 
        : WlrKeyboardFocus.None

    // Workspaces / Tags
    Rectangle { width: 1; height: 22; color: colDim; x: 14; anchors.verticalCenter: parent.verticalCenter }
    Rectangle {
        id: tagBox
        x: 28
        anchors.verticalCenter: parent.verticalCenter
        width: tagRow.width + 24
        height: 24
        color: "transparent"
        border { width: 1; color: colDim }

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
                        return (kanjiTags && n >= 1 && n <= 9) ? kanji[n - 1] : set.name
                    }
                    implicitWidth: 38
                    height: tagBox.height

                    Text {
                        anchors.fill: parent
                        text: parent.active ? "[ " + parent.label + " ]" : parent.label
                        color: parent.active ? colFg : (parent.set.urgent ? colUrgent : colDim)
                        font.family: fontFamily
                        font.pixelSize: fontSize
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
    Rectangle { width: 1; height: 22; color: colDim; x: tagBox.x + tagBox.width + 14; anchors.verticalCenter: parent.verticalCenter }

    // Center Bar Container (Title or Inline Launcher)
    Item {
        id: centerContainer
        anchors.centerIn: parent
        width: launcherOpen ? 300 : titleLabel.implicitWidth + 24
        height: 24

        Behavior on width {
            NumberAnimation { duration: 180; easing.type: Easing.OutCubic }
        }

        Rectangle {
            id: titleBox
            opacity: launcherOpen ? 0 : 1
            visible: opacity > 0
            anchors.fill: parent
            color: "transparent"
            border { width: 1; color: colDim }

            Behavior on opacity { NumberAnimation { duration: 100 } }

            Text {
                id: titleLabel
                anchors.centerIn: parent
                text: trayOpen ? "[ ZEITGEIST117 ]" : "ZEITGEIST117"
                color: colFg
                font.family: fontFamily
                font.pixelSize: fontSize
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
                    trayOpen = !trayOpen
                    batteryOpen = false
                }
            }
        }

        Rectangle {
            id: inlineLauncher
            opacity: launcherOpen ? 1 : 0
            visible: opacity > 0
            anchors.fill: parent
            color: "transparent"
            border { width: 1; color: colDim }
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
                    color: colDim
                    font.family: fontFamily
                    font.pixelSize: fontSize
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
                        color: colFg
                        font.family: fontFamily
                        font.pixelSize: fontSize
                        renderType: Text.NativeRendering
                        clip: true
                        focus: true

                        readonly property string topSuggestion: {
                            const text = text.trim()
                            if (text === "") return ""
                            const q = text.toLowerCase()
                            const match = allApps.find(app => app.toLowerCase().startsWith(q))
                            return match || ""
                        }

                        readonly property string ghostText: {
                            const txt = text
                            if (!topSuggestion || txt === "") return ""
                            if (topSuggestion.toLowerCase().startsWith(txt.toLowerCase())) {
                                return topSuggestion.substring(txt.length)
                            }
                            return ""
                        }

                        Text {
                            text: "          :COMMAND:"
                            color: colMute
                            font.family: fontFamily
                            font.pixelSize: fontSize
                            visible: !cmdInput.text && !cmdInput.inputMethodComposing
                            anchors.fill: parent
                            verticalAlignment: Text.AlignVCenter
                            horizontalAlignment: Text.AlignLeft
                            renderType: Text.NativeRendering
                        }

                        Keys.onTabPressed: {
                            if (topSuggestion) {
                                cmdInput.text = topSuggestion
                                cmdInput.cursorPosition = cmdInput.text.length
                            }
                        }
                        Keys.onReturnPressed: runLauncherCommand(cmdInput.text)
                        Keys.onEnterPressed: runLauncherCommand(cmdInput.text)
                        Keys.onEscapePressed: {
                            launcherOpen = false
                            cmdInput.text = ""
                        }

                        Text {
                            anchors.fill: parent
                            x: textMetrics.width
                            text: cmdInput.ghostText
                            color: colMute
                            font.family: fontFamily
                            font.pixelSize: fontSize
                            verticalAlignment: Text.AlignVCenter
                            renderType: Text.NativeRendering
                            visible: cmdInput.ghostText !== ""
                        }

                        TextMetrics {
                            id: textMetrics
                            font: cmdInput.font
                            text: cmdInput.text
                        }
                    }
                }

                Text {
                    text: "      ]"
                    color: colDim
                    font.family: fontFamily
                    font.pixelSize: fontSize
                    height: parent.height
                    verticalAlignment: Text.AlignVCenter
                    renderType: Text.NativeRendering
                }
            }
        }
    }

    // Right Side Modules
    Rectangle { width: 1; height: 22; color: colDim; x: rightRow.x - 14; anchors.verticalCenter: parent.verticalCenter }
    Row {
        id: rightRow
        anchors { right: parent.right; rightMargin: 30; verticalCenter: parent.verticalCenter }
        spacing: gap

        Text { text: "[CPU: " + cpuUsage + "%]"; color: colFg; font.family: fontFamily; font.pixelSize: fontSize; height: rowHeight; verticalAlignment: Text.AlignVCenter; renderType: Text.NativeRendering }
        Text { text: "[RAM: " + ramText + "]"; color: colFg; font.family: fontFamily; font.pixelSize: fontSize; height: rowHeight; verticalAlignment: Text.AlignVCenter; renderType: Text.NativeRendering }
        Text { text: "[CAL: " + Qt.formatDateTime(clock.date, "ddd MMM dd") + "]"; color: colFg; font.family: fontFamily; font.pixelSize: fontSize; height: rowHeight; verticalAlignment: Text.AlignVCenter; renderType: Text.NativeRendering }
        Text { text: "[TIME: " + Qt.formatDateTime(clock.date, "hh:mm AP") + "]"; color: colFg; font.family: fontFamily; font.pixelSize: fontSize; height: rowHeight; verticalAlignment: Text.AlignVCenter; renderType: Text.NativeRendering }
        
        Item {
            id: batContainer
            visible: hasBattery
            implicitWidth: batText.implicitWidth
            height: rowHeight
            anchors.verticalCenter: parent.verticalCenter

            Text {
                id: batText
                anchors.fill: parent
                text: isCharging ? "[CHARGING: " + battery + "%]" : "[BATTERY: " + battery + "%]"
                color: colFg
                font.family: fontFamily
                font.pixelSize: fontSize
                verticalAlignment: Text.AlignVCenter
                renderType: Text.NativeRendering 
            }

            MouseArea {
                anchors.fill: parent
                cursorShape: Qt.PointingHandCursor
                onClicked: {
                    batteryOpen = !batteryOpen
                    trayOpen = false
                }
            }
        }
    }
    Rectangle { width: 1; height: 22; color: colDim; anchors { right: parent.right; rightMargin: 14; verticalCenter: parent.verticalCenter } }

    // Bottom Border Divider Line
    Rectangle {
        anchors { left: parent.left; right: parent.right; bottom: parent.bottom }
        height: 1
        color: colMute
    }

    // Popups
    TrayPopup {}
    BatteryPopup {}
}
