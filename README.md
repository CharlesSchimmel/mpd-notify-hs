# mpd-notify-hs

* Notifications on song change, play, and pause.
* Automatic cover art detection.
* Updates wallpaper on song change.


## Install:
Requires Haskell/Stack, Feh, Libnotify\*

    git clone https://github.com/charlesschimmel/mpd-notify-hs
    cd mpd-notify-hs
    stack install

## Run:
    Usage: mpd-notify-hs --library PATH [--port INT] [--host STRING]
      Display notifications for MPD

    Available options:
      --library PATH           Path to music library
      --port INT               Port of target MPD server (default: 6600)
      --host STRING            Host of target MPD server (default: "localhost")
      -h,--help                Show this help text

\*(if you use a common desktop environment (GNOME, KDE, XFCE, etc.), it probably includes this. I like [dunst.](https://wiki.archlinux.org/index.php/Dunst))
