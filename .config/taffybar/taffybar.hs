{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import System.Taffybar (dyreTaffybarMain)
import System.Taffybar.Context (TaffybarConfig (..))
import System.Taffybar.Hooks
  ( withLogServer,
    withToggleServer,
  )
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.Information.Memory
  ( MemoryInfo (memoryUsedRatio),
    parseMeminfo,
  )
import System.Taffybar.SimpleConfig
  ( Position (Bottom, Top),
    SimpleTaffyConfig
      ( barHeight,
        barPadding,
        barPosition,
        endWidgets,
        startWidgets,
        startupHook,
        widgetSpacing
      ),
    StrutSize (ExactSize),
    toTaffyConfig,
  )
import System.Taffybar.Widget
  ( WorkspacesConfig (minIcons, showWorkspaceFn, widgetGap),
    batteryIconNew,
    buildContentsBox,
    fsMonitorNew,
    hideEmpty,
    layoutNew,
    mpris2New,
    networkGraphNew,
    sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt,
    textClockNewWith,
    windowsNew,
    workspacesNew,
  )
import System.Taffybar.Widget.Crypto (cryptoPriceLabelWithIcon, setCMCAPIKey)
import System.Taffybar.Widget.Generic.PollingGraph
  ( GraphConfig
      ( graphBackgroundColor,
        graphBorderWidth,
        graphDataColors,
        graphLabel,
        graphPadding,
        graphWidth
      ),
    pollingGraphNew,
  )
import System.Taffybar.Widget.WttrIn (textWttrNew)

main = dyreTaffybarMain myTaffybarConfig

transparent,
  yellow1,
  yellow2,
  green1,
  green2,
  taffyBlue ::
    (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9296875, 0.384375, 0.128125, 1.0)
yellow2 = (0.9765625, 0.5375, 0.1921875, 1.0)
green1 = (0.196078431, 0.392156863, 0.196078431, 1.0)
green2 = (1.0, 0.0, 1.0, 0.5)
taffyBlue = (0.070588235, 0.345098039, 0.592156863, 1.0)

myGraphConfig, memCfg :: GraphConfig
myGraphConfig =
  def
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 75,
      graphBackgroundColor = transparent
    }
memCfg =
  myGraphConfig
    { graphDataColors = [taffyBlue],
      graphLabel = Just "mem"
    }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

myTaffybarConfig :: TaffybarConfig
myTaffybarConfig =
  let myWorkspacesConfig =
        def
          { minIcons = 1,
            widgetGap = 0,
            showWorkspaceFn = hideEmpty
          }
      workspaces = workspacesNew myWorkspacesConfig
      mem = pollingGraphNew memCfg 1 memCallback
      disk = liftIO $ fsMonitorNew 500 ["/", "/tmp"]
      clock = textClockNewWith def
      layout = layoutNew def
      windowsW = windowsNew def
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig =
        def
          { startWidgets =
              workspaces : map (>>= buildContentsBox) [layout, windowsW],
            System.Taffybar.SimpleConfig.startupHook = void $ setCMCAPIKey "39c12fda-e9f4-4991-84ed-9255ff35d431",
            endWidgets =
              map
                (>>= buildContentsBox)
                [ batteryIconNew,
                  --weather,
                  clock,
                  tray,
                  --cpu,
                  mem,
                  --net,
                  disk,
                  mpris2New
                ],
            -- <> cryptos,
            barPosition = Top,
            barPadding = 10,
            barHeight = ExactSize 50,
            widgetSpacing = 0
          }
   in withLogServer . withToggleServer . toTaffyConfig $ myConfig
