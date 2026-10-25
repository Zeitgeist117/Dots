import QtQuick
import Quickshell
import Quickshell.Wayland

PanelWindow {
    anchors { top: true; bottom: true; left: true; right: true }
    color: "transparent"
    visible: barVisible
    WlrLayershell.layer: WlrLayer.Background
    WlrLayershell.keyboardFocus: WlrKeyboardFocus.None

    Item {
        anchors.fill: parent
        anchors.margins: 4

        Rectangle {
            anchors.fill: parent
            color: "transparent"
            border { width: 4; color: colMute }
        }
    }
}
