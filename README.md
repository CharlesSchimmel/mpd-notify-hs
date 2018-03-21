# mpd-notify

* Notifications on song change, play, and pause.
* Automatic cover art detection.
* Optionally updates wallpaper on song change to album artwork.
* Centers album art with a thematically colored backdrop

![mpd-notify-centered.png](mpd-notify-centered.png)
![mpd-notify-tiled.png](mpd-notify-tiled.png)

_I use [dunst](https://wiki.archlinux.org/index.php/Dunst). Your notifications may look slightly different._


## Install:
Requires [Stack](https://docs.haskellstack.org/en/stable/README/), Feh, libnotify-dev, libghc-gtk-dev

    git clone https://github.com/charlesschimmel/mpd-notify-hs
    cd mpd-notify
    stack install

## Run:
    mpd-notify-hs - Notifcations, automatic wallpapers, and more for MPD

    Usage: mpd-notify --library PATH [--port INT] [--host STRING] [--bg-style Center|Tile|None]
      Notifications for MPD

    Available options:
      --library PATH           Path to music library
      --port INT               Port of target MPD server (default: 6600)
      --host STRING            Host of target MPD server (default: "localhost")
      --bg-style Center|Tile|None
                               Fill type for displaying the album art as a background (default: Center)
      -h,--help                Show this help text
