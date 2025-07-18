/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int gappx     = 10;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayonleft = 0;   	/* 0: systray in the right corner, >0: systray on left of status text */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const int showbar            = 1;     /* 0 means no bar */
static const int topbar             = 1;     /* 0 means bottom bar */
static const char *fonts[]          = { "Monaspace Krypton:size=14", "FontAwesome:size=14", "JoyPixels:pixelsize=14", "Sazanami Mincho:size=14" };
static const char col_gray1[]       = "#282828";
static const char col_gray2[]       = "#282828";
static const char col_gray3[]       = "#ebdbb2";
static const char col_gray4[]       = "#282828";
static const char col_cyan[]        = "#ebdbb2";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
};

typedef struct {
	const char *name;
	const void *cmd;
} Sp;
/* const char *spcmd1[] = {"alacritty", "-c", "spnc", NULL }; */
/* const char *spcmd2[] = {"alacritty", "-c", "sppm", "-e", "pulsemixer", NULL }; */
/* const char *spcmd3[] = {"alacritty", "-c", "spbt", "-e", "btop", NULL }; */
/* const char *spcmd4[] = {"alacritty", "-c", "spst", NULL }; */
const char *spcmd1[] = {"st", "-n", "spnc", "-g", "100x30", NULL };
const char *spcmd2[] = {"st", "-n", "sppm", "-g", "100x30", "-e", "pulsemixer", NULL };
const char *spcmd3[] = {"st", "-n", "spbt", "-g", "100x40", "-e", "btop", NULL };
const char *spcmd4[] = {"st", "-n", "spst", "-g", "100x30", NULL };
const char *spcmd5[] = {"st", "-n", "spmc", "-g", "80x40", "-e", "kew", NULL };
static Sp scratchpads[] = {
	/* name          cmd  */
	{"spnc",		spcmd1},
	{"sppm",		spcmd2},
	{"spbt",		spcmd3},
	{"spst",		spcmd4},
	{"spmc",		spcmd5},
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9"};
/* static const char *tags[] = { "一", "二", "三", "四", "五", "六", "七", "八", "九"}; */

/*Window Rules*/
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class                    instance    title           tags mask  isfloating  monitor */
	{ "Steam",                  NULL,       NULL,           0,         1,          -1 },
	{ "mpv",                    NULL,       NULL,           0,         1,          -1 },
	{ "Gimp",                   NULL,       NULL,           0,         0,          -1 },
	{ "St",                     NULL,       NULL,           0,         0,          -1 },
	{ NULL,		                NULL,       "Event Tester", 0,         0,          -1 }, /* xev */
	{ "MEGAsync",               NULL,       NULL,           0,         1,          -1 },
	{ "Proton Mail Bridge",     NULL,       NULL,           0,         1,          -1 },
	{ NULL,		                "spnc",		NULL,	        SPTAG(0),  1,		   -1 },
	{ NULL,		                "sppm",		NULL,	        SPTAG(1),  1,		   -1 },
	{ NULL,		                "spbt",		NULL,	        SPTAG(2),  1,		   -1 },
	{ NULL,		                "spst",		NULL,	        SPTAG(3),  1,		   -1 },
	{ NULL,		                "spmc",		NULL,	        SPTAG(4),  1,		   -1 },
};

/* layout(s) */
static const float mfact     = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "[F]=",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run","-p", "󰘧", "-c", "-l", "15", NULL };
static const char *pmenucmd[] = { "passmenu","-c", "-l", "20", NULL };
static const char *termcmd[]  = { "alacritty", NULL };
static const char *filecmd[]  = { "thunar", NULL };
static const char *emacscmd[]  = { "emacsclient", "-c", "-a", "emacs", NULL };
static const char *ecmd[] = { "firefox", NULL };
static const char *scrwcmd[] = { "scr", "select",  NULL };
static const char *scrcmd[] = { "scr",  NULL };


#include <X11/XF86keysym.h>
#include "movestack.c"
#include "shiftview.c"

/* Key Bindings */
static Key keys[] = {
  /* modifier                       key                  function        argument */
	{ MODKEY,                       XK_space,            spawn,           {.v = dmenucmd } },
	{ MODKEY,			            XK_Return,           spawn,           {.v = termcmd } },
	{ MODKEY,			            XK_apostrophe,       spawn,       {.v = emacscmd } },
	{ MODKEY,                       XK_q,                spawn,          {.v = filecmd} },
	{ MODKEY,                       XK_b,                togglebar,      {0} },
	{ MODKEY,                       XK_j,                focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,                focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_i,                incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_d,                incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,                setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,                setmfact,       {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_j,                movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_k,                movestack,      {.i = -1 } },
	{ MODKEY,                       XK_Return,           zoom,           {0} },
	{ MODKEY,                       XK_Tab,              view,           {0} },
	{ MODKEY,                       XK_w,                killclient,     {0} },
	{ MODKEY,                       XK_t,                setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,                setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,                setlayout,      {.v = &layouts[2]} },
	{ MODKEY,		                XK_v,                spawn,	        {.v = ecmd} },
	{ MODKEY,		                XK_e,                spawn,          {.v = pmenucmd} },
	{ MODKEY|ControlMask,           XK_space,            setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,            togglefloating, {0} },
	{ MODKEY,                       XK_0,                view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,                tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,            focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period,           focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,            tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period,           tagmon,         {.i = +1 } },
	{ MODKEY,                       XK_minus,            setgaps,        {.i = -1 } },
	{ MODKEY,                       XK_equal,            setgaps,        {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_equal,            setgaps,        {.i = 0  } },
	{ MODKEY|ShiftMask,	        	XK_h,	             shiftview,	    {.i = -1} },
	{ MODKEY|ShiftMask,		        XK_l,	             shiftview,	    {.i = +1} },
	{ MODKEY,			            Button4,             shiftview,      {.i = +1} },
	{ MODKEY,			            Button5,             shiftview,      {.i = -1} },
	{ MODKEY,            	    	XK_o,	             togglescratch,  {.ui = 0 } },
	{ MODKEY,               		XK_p,	             togglescratch,  {.ui = 1 } },
	{ MODKEY,                		XK_g,	             togglescratch,  {.ui = 2 } },
	{ MODKEY,                		XK_i,	             togglescratch,  {.ui = 3 } },
	{ MODKEY,                		XK_n,	             togglescratch,  {.ui = 4 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,                quit,           {0} },
	/*Audio Keys*/
	{ 0, XF86XK_AudioMute,			             spawn,		SHCMD("pamixer -t && getvol") },
	{ 0, XF86XK_AudioRaiseVolume,	                     spawn,		SHCMD("pamixer -i 5 && getvol") },
	{ 0, XF86XK_AudioLowerVolume,	                     spawn,		SHCMD("pamixer -d 5 && getvol") },

	/*Media Keys*/
	{ 0, XF86XK_AudioPlay,	                             spawn,		SHCMD("playerctl -p kew,DeaDBeeF,Feishin play-pause") },
	{ 0, XF86XK_AudioNext,                               spawn,		SHCMD("playerctl -p kew,DeaDBeeF,Feishin next") },
	{ 0, XF86XK_AudioPrev,                               spawn,		SHCMD("playerctl -p kew,DeaDBeeF,Feishin previous") },

	{ 0, XF86XK_MonBrightnessUp,	                     spawn,		SHCMD("brightnessctl s 10%+") },
	{ 0, XF86XK_MonBrightnessDown,	                     spawn,		SHCMD("brightnessctl s 10%-") },

	{ 0, XK_Print,	                                     spawn,     {.v = scrwcmd } },
	{ 0|ShiftMask, XK_Print,	                         spawn,     {.v = scrcmd } },

	{ MODKEY, XK_s,	                                     spawn,     {.v = scrwcmd } },
	{ MODKEY|ShiftMask, XK_s,	                         spawn,     {.v = scrcmd } },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
