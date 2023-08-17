import Graphics.X11.ExtraTypes (xF86XK_PowerDown)
import XMonad
  ( Full (Full),
    Mirror (Mirror),
    Tall (Tall),
    XConfig (keys, terminal),
    handleEventHook,
    layoutHook,
    spawn,
    xmonad,
    (|||), Default (def),
  )
import XMonad.Actions.ShowText (handleTimerEvent, flashText)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobarProp)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar ()
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Actions.CycleWS (nextWS)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig =
  desktopConfig
    { terminal = "alacritty",
      layoutHook = myLayout,
      handleEventHook = handleTimerEvent
    }
    `additionalKeysP` [ ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "vimb"),
                        ("<XF86PowerOff>", spawn "systemctl suspend"),
                        ("<XF86AudioRaiseVolume>", spawn "pamixer sset Master 10%+"),
                        ("<XF86AudioLowerVolume>", spawn "pamixer sset Master 10%-"),
                        ("<XF86AudioMute>", spawn "pamixer sset Master toggle"),
                        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
                        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
                      ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100
