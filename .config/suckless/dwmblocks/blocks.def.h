//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{"[", "/home/nightwing/.config/suckless/dwmblocks/scripts/forecast.sh", 1800, 5},
	{"﬙ ", "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g",	10,		0},
	{" ", "top -bn1 | grep Cpu | awk '{print $2}'", 5, 0},
	{" ", "df -h /home/nightwing| grep -vE '^Filesystem' | awk '{print $5, $4 }'", 3600, 0},
	{"", "/home/nightwing/.config/suckless/dwmblocks/scripts/date.sh", 1,	0},
	{"", "/home/nightwing/.config/suckless/dwmblocks/scripts/bat.sh", 1,	0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = "][";
static unsigned int delimLen = 5;
