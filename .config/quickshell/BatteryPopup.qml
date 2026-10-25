import QtQuick
import Quickshell
import Quickshell.Services.UPower

PopupWindow {
    id: batteryPopup
    visible: batteryOpen && hasBattery
    color: "transparent"
    anchor.window: parent
    anchor.rect.x: parent.width - 220
    anchor.rect.y: parent.height + 2
    implicitWidth: 380
    implicitHeight: batMainCol.height + 36
    grabFocus: false

    onVisibleChanged: {
        if (!visible && batteryOpen) {
            batteryOpen = false
        }
    }

    Rectangle {
        anchors.fill: parent
        color: colBg
        border { width: 1; color: colDim }

        Column {
            id: batMainCol
            x: 18; y: 18
            width: parent.width - 36
            spacing: 14

            Text {
                text: "BATTERY :: " + (isCharging ? "Charging" : "Discharging")
                color: colFg
                font.family: fontFamily
                font.pixelSize: fontSize
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
                            color: (index / 15 < (battery / 100)) ? colBatActive : colBatInactive
                        }
                    }
                }

                Text {
                    text: battery + "%"
                    color: colFg
                    font.family: fontFamily
                    font.pixelSize: fontSize + 4
                    font.bold: true
                    height: 16
                    verticalAlignment: Text.AlignVCenter
                    renderType: Text.NativeRendering
                }
            }

            Column {
                spacing: 4
                Text {
                    text: (isCharging ? "Time to Full: " : "Time Left: ") + formatDuration(isCharging ? timeToFull : timeToEmpty)
                    color: colFg
                    font.family: fontFamily
                    font.pixelSize: fontSize - 1
                    renderType: Text.NativeRendering
                }
                Text {
                    text: (isCharging ? "Charging: +" : "Discharging: ") + energyRate.toFixed(1) + "W"
                    color: colFg
                    font.family: fontFamily
                    font.pixelSize: fontSize - 1
                    renderType: Text.NativeRendering
                }
            }
        }
    }
}
