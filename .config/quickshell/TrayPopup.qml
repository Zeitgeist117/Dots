import QtQuick
import QtQuick.Effects
import Quickshell
import Quickshell.Services.SystemTray

PopupWindow {
    id: trayPopup
    visible: trayOpen
    color: "transparent"
    anchor.window: parent
    anchor.rect.x: parent.width / 2 - width / 2
    anchor.rect.y: parent.height + 2
    implicitWidth: 260
    implicitHeight: Math.min(400, trayCol.height + 16)
    grabFocus: true

    onVisibleChanged: {
        if (!visible && trayOpen) {
            trayOpen = false
        }
    }

    Rectangle {
        anchors.fill: parent
        color: colBg
        border { width: 1; color: colDim }

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
                    height: rowHeight
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    acceptedButtons: Qt.LeftButton | Qt.RightButton | Qt.MiddleButton

                    Rectangle {
                        anchors.fill: parent
                        color: trayItem.containsMouse ? colMute : "transparent"
                    }
                    Image {
                        id: iconImg
                        x: 6
                        anchors.verticalCenter: parent.verticalCenter
                        width: 16; height: 16
                        source: trayItem.modelData.icon
                        sourceSize: Qt.size(32, 32)
                        smooth: true
                        visible: !trayTint
                    }
                    MultiEffect {
                        anchors.fill: iconImg
                        source: iconImg
                        visible: trayTint
                        saturation: -1.0
                        brightness: traySilhouette ? 1.0 : 0.25
                        colorization: 1.0
                        colorizationColor: colFg
                    }
                    Text {
                        anchors { left: iconImg.right; leftMargin: 10; right: parent.right; rightMargin: 6 }
                        text: (trayItem.modelData.tooltipTitle || trayItem.modelData.title || trayItem.modelData.id).toUpperCase()
                        color: colFg
                        font.family: fontFamily
                        font.pixelSize: fontSize
                        height: rowHeight
                        verticalAlignment: Text.AlignVCenter
                        renderType: Text.NativeRendering
                        elide: Text.ElideRight
                    }
                    onClicked: mouse => {
                        if (mouse.button === Qt.MiddleButton) {
                            modelData.secondaryActivate()
                        } else if (mouse.button === Qt.RightButton || (modelData.onlyMenu && modelData.hasMenu)) {
                            if (modelData.hasMenu && modelData.menu) {
                                activeMenuHandle = modelData.menu
                                activeTrayItem = modelData
                                trayOpen = false
                                Qt.callLater(() => nativeMenuAnchor.open())
                            }
                        } else {
                            modelData.activate()
                            trayOpen = false
                        }
                    }
                }
            }
        }
    }

    QsMenuAnchor {
        id: nativeMenuAnchor
        menu: activeMenuHandle
        anchor.window: parent
        anchor.rect.x: parent.width / 2 - 130
        anchor.rect.y: parent.height + 2

        onClosed: {
            activeTrayItem = null
            activeMenuHandle = null
        }
    }
}
