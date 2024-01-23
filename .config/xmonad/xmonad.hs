import Graphics.X11.ExtraTypes (xF86XK_PowerDown)
import System.Taffybar (dyreTaffybar)
import System.Taffybar.Context (defaultTaffybarConfig)
import System.Taffybar.SimpleConfig (defaultSimpleTaffyConfig, toTaffyConfig)
import System.Taffybar.Support.PagerHints (pagerHints)
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
    io,
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
import XMonad.Actions.WindowGo (raise)
import XMonad.Config (defaultConfig)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
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
main = xmonad . docks . ewmhFullscreen . ewmh . pagerHints $ myConfig

myConfig =
  gnomeConfig
    { terminal = "alacritty",
      layoutHook = myLayout,
      normalBorderColor = "#3B4252",
      focusedBorderColor = "#4C566A",
      startupHook = do
        -- TODO move process to systemd
        --spawn "compfy"
        spawn "taffybar"
        spawn "feh --bg-fill --randomize ~/Wallpapers/*"
        --spawn "/opt/discord/Discord --start-minimized"
        --spawn "/opt/enpass/Enpass -minimize"
        --spawn "blueman-applet"
    }
    `additionalKeysP` [ ("M-q", spawn "gnome-session-quit"),
                        ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "brave"),
                        ("M-s", runSteam),
                        ("<XF86PowerOff>", spawn "systemctl suspend"),
                        ("<XF86AudioRaiseVolume>", spawn "pamixer sset Master 10%+"),
                        ("<XF86AudioLowerVolume>", spawn "pamixer sset Master 10%-"),
                        ("<XF86AudioMute>", spawn "pamixer sset Master toggle"),
                        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
                        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
                      ]

myLayout = tiled ||| full ||| mirror ||| grid
  where
    tiled = avoidStruts $ Tall nmaster delta ratio
    full = avoidStruts Full
    mirror = avoidStruts $ Mirror tiled
    grid = avoidStruts Grid
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100

runSteam = do
  spawn "xrandr --output HDMI-1 --mode 1920x1080 --primary --left-of eDP-1"
  --spawn "xrandr --output eDP-1 --off"
  spawn "steam -windowed"
  raise $ className =? "steam"
  sendMessage $ JumpToLayout "Full"
