/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int gappx     = 10;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayonleft = 0;   	/* 0: systray in the right corner, >0: systray on left of status text */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const int showbar            = 1;     /* 0 means no bar */
static const int topbar             = 1;     /* 0 means bottom bar */
static const char *fonts[]          = { "Cascadia Code:size=15", "FontAwesome:size=15", "JoyPixels:pixelsize=15", "Sazanami Mincho:size=15" };
static const char col_gray1[]       = "#282A36";
static const char col_gray2[]       = "#282A36";
static const char col_gray3[]       = "#f8f8f2";
static const char col_gray4[]       = "#282A36";
static const char col_cyan[]        = "#f8f8f2";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
};

typedef struct {
	const char *name;
	const void *cmd;
} Sp;
const char *spcmd1[] = {"st", "-n", "spnews", "-g", "100x30", "newsboat", NULL };
const char *spcmd2[] = {"st", "-n", "spranger", "-g", "100x30", "-e", "ranger", NULL };
const char *spcmd3[] = {"st", "-n", "spmd", "-g", "100x30", "-e", "nvim", "/home/nightwing/Notes/wiki/index.wiki", NULL };
const char *spcmd4[] = {"st", "-n", "spnc", "-g", "100x30", "-e", "ncmpcpp", NULL };
const char *spcmd5[] = {"st", "-n", "sppm", "-g", "100x30", "-e", "pulsemixer", NULL };
const char *spcmd6[] = {"st", "-n", "spbt", "-g", "100x30", "-e", "btop", NULL };
static Sp scratchpads[] = {
	/* name          cmd  */
	{"spnews",      spcmd1},
	{"spranger",    spcmd2},
	{"spmd",		spcmd3},
	{"spnc",		spcmd4},
	{"sppm",		spcmd5},
	{"spbt",		spcmd6},
};

/* tagging */
static const char *tags[] = { "", "", "", "", "", "", ""};

/*Window Rules*/
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class                    instance    title           tags mask  isfloating  monitor */
	{ "Steam",                  NULL,       NULL,           0,         1,          -1 },
	{ "Gimp",                   NULL,       NULL,           0,         0,          -1 },
	{ "St",                     NULL,       NULL,           0,         0,          -1 },
	{ NULL,		                NULL,       "Event Tester", 0,         0,          -1 }, /* xev */
	{ "MEGAsync",               NULL,       NULL,           0,         1,          -1 },
	{ "Proton Mail Bridge",     NULL,       NULL,           0,         1,          -1 },
	{ NULL,		                "spnews",	NULL,	        SPTAG(0),  1,		   -1 },
	{ NULL,		                "spranger",	NULL,	        SPTAG(1),  1,		   -1 },
	{ NULL,		                "spmd",		NULL,	        SPTAG(2),  1,		   -1 },
	{ NULL,		                "spnc",		NULL,	        SPTAG(3),  1,		   -1 },
	{ NULL,		                "sppm",		NULL,	        SPTAG(4),  1,		   -1 },
	{ NULL,		                "spbt",		NULL,	        SPTAG(5),  1,		   -1 },
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
static const char *dmenucmd[] = { "dmenu_run","-p", "󰘧", "-c", "-l", "20", NULL };
static const char *wmenucmd[] = { "watchmenu", NULL };
static const char *pmenucmd[] = { "passmenu","-c", "-l", "20", NULL };
static const char *termcmd[]  = { "st", NULL };
static const char *emacscmd[]  = { "emacsclient", "-c", "-a", "emacs", NULL };
static const char *qbrowser[]  = { "qutebrowser", NULL };
static const char *ecmd[] = { "brave", NULL };
static const char *scrwcmd[] = { "scr", "select",  NULL };
static const char *scrcmd[] = { "scr",  NULL };


#include <X11/XF86keysym.h>
#include "movestack.c"
#include "shiftview.c"

/* Key Bindings */
static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_space,  spawn,          {.v = dmenucmd } },
	{ MODKEY,						XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,						XK_apostrophe, spawn,          {.v = emacscmd } },
	{ MODKEY,						XK_q,	   spawn,          {.v = qbrowser } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_j,      movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_k,      movestack,      {.i = -1 } },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY,                       XK_w,      killclient,     {0} },
	{ MODKEY|ShiftMask,             XK_w,      spawn,          {.v = wmenucmd } },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_r,      setlayout,      {.v = &layouts[3]} },
	{ MODKEY,						XK_v,      spawn,	       {.v = ecmd} },
	{ MODKEY,						XK_e,      spawn,          {.v = pmenucmd} },
	{ MODKEY,			            XK_y,      setlayout,      {.v = &layouts[6]} },
	{ MODKEY|ControlMask,           XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY,                       XK_minus,  setgaps,        {.i = -1 } },
	{ MODKEY,                       XK_equal,  setgaps,        {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_equal,  setgaps,        {.i = 0  } },
	{ MODKEY,						XK_bracketleft,		shiftview,	   {.i = -1} },
	{ MODKEY,						XK_bracketright,	shiftview,	   {.i = +1} },
	{ MODKEY,						Button4,	shiftview, {.i = +1} },
	{ MODKEY,						Button5,	shiftview, {.i = -1} },
	{ MODKEY,            			XK_c,  	   togglescratch,  {.ui = 0 } },
	{ MODKEY,            			XK_x,	   togglescratch,  {.ui = 1 } },
	{ MODKEY,            			XK_z,	   togglescratch,  {.ui = 2 } },
	{ MODKEY,            			XK_n,	   togglescratch,  {.ui = 3 } },
	{ MODKEY,            			XK_p,	   togglescratch,  {.ui = 4 } },
	{ MODKEY,            			XK_g,	   togglescratch,  {.ui = 5 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
	/*Audio Keys*/
	/* { 0, XF86XK_AudioMute,			spawn,		SHCMD("pamixer -t; kill -44 $(pidof dwmblocks)") }, */
	/* { 0, XF86XK_AudioRaiseVolume,	spawn,		SHCMD("pamixer --allow-boost -i 5; kill -44 $(pidof dwmblocks)") }, */
	/* { 0, XF86XK_AudioLowerVolume,	spawn,		SHCMD("pamixer --allow-boost -d 5; kill -44 $(pidof dwmblocks)") }, */

	{ 0, XF86XK_AudioMute,			spawn,		SHCMD("pamixer -t && getvol") },
	{ 0, XF86XK_AudioRaiseVolume,	spawn,		SHCMD("pamixer -i 5 && getvol") },
	{ 0, XF86XK_AudioLowerVolume,	spawn,		SHCMD("pamixer -d 5 && getvol") },

	{ 0, XF86XK_MonBrightnessUp,	spawn,		SHCMD("brightnessctl s 10%+") },
	{ 0, XF86XK_MonBrightnessDown,	spawn,		SHCMD("brightnessctl s 10%-") },

	{ 0, XK_Print,	   spawn,  {.v = scrwcmd } },
	{ 0|ShiftMask, XK_Print,	   spawn,  {.v = scrcmd } },
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


