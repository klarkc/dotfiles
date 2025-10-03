import Graphics.X11.ExtraTypes (xF86XK_PowerDown)
import System.Taffybar (dyreTaffybar)
import System.Taffybar.Context (defaultTaffybarConfig)
import System.Taffybar.SimpleConfig (defaultSimpleTaffyConfig, toTaffyConfig)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
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
import XMonad.Config.Desktop (desktopConfig)
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
import XMonad.Layout.Tabbed (TabbedDecoration (Tabbed), shrinkText, simpleTabbed, tabbed, tabbedLeft, tabbedRight, simpleTabbedLeft)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.ToggleLayouts (ToggleLayout (ToggleLayout))
import XMonad.Operations (unGrab)
import XMonad.StackSet (hidden, tag)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Themes (darkTheme, theme)

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh . pagerHints $ myConfig

myConfig =
  def
    { keys = keys desktopConfig,
      terminal = "alacritty",
      layoutHook = myLayout,
      normalBorderColor = "#3B4252",
      focusedBorderColor = "#5E81AC",
      startupHook = do
        spawn "xsetroot -cursor_name left_ptr"
        spawn "taffybar"
        spawn "picom"
        spawn "feh --bg-fill --randomize ~/Wallpapers/*"
        spawn "/opt/discord/Discord --start-minimized"
        spawn "/opt/enpass/Enpass -minimize"
        spawn "blueman-applet"
        spawn "solaar -w hidden"
        --spawn "steam-runtime -silent"
    }
    `additionalKeysP` [ ("M-q", spawn "gnome-session-quit --logout --no-prompt"),
                        ("M-C-s", unGrab *> spawn "scrot -s -F - | satty --filename - --fullscreen --early-exit --action-on-enter save-to-file --initial-tool highlight --output-filename ~/screenshot-$(date '+%Y%m%d-%H:%M:%S').png"),
                        ("M-f", spawn "vimb"),
                        ("M-s", runSteam),
                        ("<XF86PowerOff>", spawn "systemctl suspend"),
                        ("<XF86AudioRaiseVolume>", spawn "pamixer sset Master 10%+"),
                        ("<XF86AudioLowerVolume>", spawn "pamixer sset Master 10%-"),
                        ("<XF86AudioMute>", spawn "pamixer sset Master toggle"),
                        ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%"),
                        ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
                      ]

myLayout = tiled ||| full ||| mirror ||| tabbed
  where
    tiled = avoidStruts $ Tall nmaster delta ratio
    full = avoidStruts Full
    mirror = avoidStruts $ Mirror tiled
    tabbed = avoidStruts simpleTabbedLeft
    nmaster = 1
    ratio = 2 / 3
    delta = 3 / 100

runSteam = do
  spawn "xrandr --output HDMI-1 --mode 1920x1080 --primary --left-of eDP-1"
  -- spawn "xrandr --output eDP-1 --off"
  spawn "steam steam://open/bigpicture"
  raise $ className =? "steam"
  sendMessage $ JumpToLayout "Full"
