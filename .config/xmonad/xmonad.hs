import XMonad
  ( Full (Full),
    Mirror (Mirror),
    Tall (Tall),
    XConfig (terminal),
    layoutHook,
    spawn,
    xmonad,
    (|||),
  )
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobarProp)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar ()
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab (unGrab)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig =
  desktopConfig
    { terminal = "alacritty",
      layoutHook = myLayout
    }
    `additionalKeysP` [ ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "vimb")
                      ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100
