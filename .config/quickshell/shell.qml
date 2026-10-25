//@ pragma UseQApplication

import QtQuick
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import Qt.labs.platform

Scope {
    id: root

    // ── Shared Theme & Global Properties ─────────────────────────────────
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

    // ── Shared UI State ──────────────────────────────────────────────────
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

    // ── System Processes & Timers ────────────────────────────────────────
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

    // ── Instantiate Modular Windows ──────────────────────────────────────
    BorderWindow {}
    TopBar {}
}
