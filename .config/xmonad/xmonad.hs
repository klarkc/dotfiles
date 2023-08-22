import Graphics.X11.ExtraTypes (xF86XK_PowerDown)
import XMonad
  ( Default (def),
    Full (Full),
    Mirror (Mirror),
    Tall (Tall),
    XConfig (keys, terminal),
    handleEventHook,
    layoutHook,
    spawn,
    startupHook,
    xmonad,
    (|||), normalBorderColor, focusedBorderColor,
  )
import XMonad.Actions.CycleWS (nextWS)
import XMonad.Actions.ShowText (flashText, handleTimerEvent)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobarProp)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.StatusBar ()
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.Tabbed (tabbedLeft, simpleTabbed, shrinkText, tabbed, tabbedRight)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.Themes (darkTheme, theme)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig =
  desktopConfig
    { terminal = "alacritty",
      layoutHook = myLayout,
      normalBorderColor = "#3B4252",
      focusedBorderColor = "#4C566A",
      startupHook = do
        spawn "feh --bg-fill --randomize ~/Wallpapers/*"
    }
    `additionalKeysP` [ ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "brave"),
                        ("M-s", spawn "steam"),
                        ("<XF86PowerOff>", spawn "systemctl suspend"),
                        ("<XF86AudioRaiseVolume>", spawn "pamixer sset Master 10%+"),
                        ("<XF86AudioLowerVolume>", spawn "pamixer sset Master 10%-"),
                        ("<XF86AudioMute>", spawn "pamixer sset Master toggle"),
                        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
                        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
                      ]

myLayout =  tiled |||  Full ||| Mirror tiled ||| tabbedRight shrinkText (theme darkTheme) ||| threeCol 
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100
