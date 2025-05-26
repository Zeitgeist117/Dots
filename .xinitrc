#!/bin/sh
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

session=${1:-dwm}

xrdb ~/.Xresources
case $session in
    dwm    ) exec dwm;;
    sxwm    ) exec sxwm;;
    bspwm  ) exec bspwm;;
    xmonad ) exec xmonad;;
    # qtile  ) exec startqt;;
    # awm	   ) exec awesome;;
    # fvwm   ) exec fvwm3;;
    # i3     ) exec i3;;
    # gnome  ) export XDG_SESSION_TYPE=x11 && export GDK_BACKEND=x11 && exec gnome-session && exec syncthing;;
    kde	   ) export DESKTOP_SESSION=plasma && exec startplasma-x11 ;;
    xfce   ) export XDG_SESSION_TYPE=x11 && exec startxfce4 ;;
    # icewm  ) export XDG_SESSION_TYPE=x11 && exec icewm-session ;;
    *      ) exec $1;;
esac
