import Graphics.X11.ExtraTypes (xF86XK_PowerDown)
import XMonad
  ( Default (def),
    Full (Full),
    Layout (Layout),
    Mirror (Mirror),
    Tall (Tall),
    XConfig (keys, terminal),
    XState (windowset),
    className,
    composeAll,
    doIgnore,
    doShift,
    focusedBorderColor,
    gets,
    handleEventHook,
    layoutHook,
    liftX,
    manageHook,
    normalBorderColor,
    sendMessage,
    spawn,
    startupHook,
    xmonad,
    (-->),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CycleWS (nextWS)
import XMonad.Actions.ShowText (flashText, handleTimerEvent)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (xmobarProp)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.StatusBar ()
import XMonad.Hooks.StatusBar.PP ()
import XMonad.Layout (JumpToLayout (JumpToLayout))
import XMonad.Layout.Decoration (ModifiedLayout (ModifiedLayout))
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.IfMax (IfMax (IfMax))
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.MultiToggle (single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.Tabbed (shrinkText, simpleTabbed, tabbed, tabbedLeft, tabbedRight)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.ToggleLayouts (ToggleLayout (ToggleLayout))
import XMonad.StackSet (hidden, tag)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Themes (darkTheme, theme)
import XMonad.Util.Ungrab (unGrab)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . xmobarProp $ myConfig

myConfig =
  desktopConfig
    { terminal = "alacritty",
      layoutHook = myLayout,
      normalBorderColor = "#3B4252",
      focusedBorderColor = "#4C566A",
      startupHook = do
        spawn "gnome-keyring-daemon --start --components=pkcs11"
        spawn "gnome-keyring-daemon --start --components=ssh"
        spawn "gnome-keyring-daemon --start --components=secrets"
        spawn "xfsettingsd"
        spawn "feh --bg-fill --randomize ~/Wallpapers/*"
        spawn "/opt/discord/Discord --start-minimized"
        spawn "/opt/enpass/Enpass -minimize"
    }
    `additionalKeysP` [ ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "brave"),
                        ("M-s", runSteam),
                        ("<XF86PowerOff>", spawn "systemctl suspend"),
                        ("<XF86AudioRaiseVolume>", spawn "pamixer sset Master 10%+"),
                        ("<XF86AudioLowerVolume>", spawn "pamixer sset Master 10%-"),
                        ("<XF86AudioMute>", spawn "pamixer sset Master toggle"),
                        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
                        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
                      ]

myLayout =
  tiled
    ||| Full
    ||| Mirror tiled
    ||| Grid
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100

runSteam = do
  spawn "steam -windowed"
  sendMessage $ JumpToLayout "Full"
