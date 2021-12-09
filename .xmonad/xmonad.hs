-- #------------------------------------------------------------------------------------------------------------------------------------------------------
-- #                 PROJECT NAME: XMONAD   
-- #                      
-- # Config file:        ~/.xmonad/xmoad.hs
-- # Started On:         Wed  8 Sep 19:33:31 CDT 2021
-- # Last Change:        Wed  8 Sep 19:33:31 CDT 2021
-- # Author E-Mail:      jake@jpedmedia.com
-- # Author GitHub:      https://github.com/jdpedersen1
-- # Author Gitlab:      https://gitlab.com/jped
-- #------------------------------------------------------------------------------

-- IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

 -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Tree

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.

    --Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
--import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.SpawnOn

    -- Layouts modifiers
import XMonad.Layout.Accordion
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral 
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
myFont :: [Char]
myFont = "xft:Roboto:style=Bold:pixelsize=13"

myModMask :: KeyMask
myModMask = mod1Mask       -- Sets modkey to left alt key

myTerminal :: [Char]
myTerminal = "kitty"   -- Sets default terminal

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: [Char]
myNormColor   = "#292d3e"  -- Border color of normal windows

myFocusColor :: [Char]
myFocusColor    = "#477d8f"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

altMask :: KeyMask
altMask = mod4Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
        --Background--
          spawnOnce "xwallpaper --zoom ~/Pictures/wallpaper/brand_wallpapers/jakewall.png"
       -- spawnOnce "nitrogen --random --set-scaled &" 

        --System--
        --spawnOnce "conky"
          spawnOnce "picom --experimental-backend &"
          spawnOnce "xfce4-power-manager &"
       -- spawnOnce "lxsession &"
      
        --Sys tray--
          spawnOnce "trayer --edge top --align right --widthtype request --padding 0 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x222222 --height 24 &"
          spawnOnce "mailspring -b &"
       -- spawnOnce "pamac-tray-appindicator"
       -- spawnOnce "nm-applet &"
       -- spawnOnce "volumeicon &"

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ranger" spawnRanger findRanger manageRanger
                , NS "ncmpcpp" spawnNcmpcpp findNcmpcpp manageNcmpcpp
                , NS "calculator" spawnCalc findCalc manageCalc
                , NS "mail" spawnMail findMail manageMail
                , NS "edit" spawnEdit findEdit manageEdit
                , NS "keys" spawnKeys findKeys manageKeys
                ]
    where
        spawnTerm  = myTerminal ++ " -T scratchpad"
        findTerm   = title =? "scratchpad"
        manageTerm = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.6
                        w = 0.6
                        t = 0.7 -h 
                        l = 0.8 -w

        spawnRanger  = myTerminal ++ " -T ranger -e ranger"
        findRanger   = title =? "ranger"
        manageRanger = customFloating $ W.RationalRect l t w h
                    where
                        h = 0.6
                        w = 0.6
                        t = 0.7 -h 
                        l = 0.8 -w

        spawnNcmpcpp  = myTerminal ++ " -T ncmpcpp -e ncmpcpp"
        findNcmpcpp   = title =? "ncmpcpp"
        manageNcmpcpp = customFloating $ W.RationalRect l t w h
                       where
                           h = 0.5
                           w = 1.0
                           t = 0.53 -h
                           l = 1.0 -w
        spawnCalc  = "qalculate-gtk"
        findCalc   = className =? "Qalculate-gtk"
        manageCalc = customFloating $ W.RationalRect l t w h
                       where
                           h = 0.6
                           w = 0.75
                           t = 0.60 -h
                           l = 0.87 -w
        spawnMail  = "mailspring"
        findMail   = className =? "Mailspring"
        manageMail = customFloating $ W.RationalRect l t w h
                       where
                           h = 0.5
                           w = 1.0
                           t = 0.53 -h
                           l = 1.0 -w

        spawnEdit  = myTerminal ++ " -T testmenu.sh -e testmenu.sh"
        findEdit   = title =? "testmenu.sh"
        manageEdit = customFloating $ W.RationalRect l t w h
                       where
                           h = 0.5
                           w = 1.0
                           t = 0.53 -h
                           l = 1.0 -w

        spawnKeys  = myTerminal ++ " -T hotkeys.sh --hold -e hotkeys.sh"
        findKeys   = title =? "hotkeys.sh"
        manageKeys = customFloating $ W.RationalRect l t w h
                       where
                           h = 0.9
                           w = 0.6
                           t = 0.95 -h
                           l = 0.8 -w


------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x29,0x75,0x98) -- lowest inactive bg
                  (0x29,0x43,0x99) -- highest inactive bg
                  (0x55,0x0,0x0) -- active bg
                  (0x29,0x68,0x12) -- inactive fg
                  (0x0,0x50,0x55) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }


-- Set favorite apps for the spawnSelected'
myAppGrid :: [([Char], [Char])]
myAppGrid = [ ("Firefox", "firefox")
            , ("Gimp", "gimp")
            , ("KDEnlive", "kdenlive")
            , ("Simple Terminal", "st")
            , ("Cleanup", "cleanup")
            , ("Virt-Manager", "virt-manager")
            , ("Nemo", "nemo")
            , ("Mail", "mailspring")
            , ("Ranger", "st -e ranger")
            , ("Brave", "brave")
            , ("Wallpaper", "wall")
            , ("Update", "alacritty -e yay -Syyu")
            , ("Qutebrowser", "qutebrowser")
            ]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = "xft:RobotoFont:size=9"
      , bgColor             = "#222222"
      , fgColor             = "#bbbbbb"
      , bgHLight            = "#477d8f"
      , fgHLight            = "#222222"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
--      , promptKeymap        = dtXPKeymap
--      , position            = Bottom
--      , position            = Top
      , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = isPrefixOf
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config minus the autocomplete feature which is annoying on
-- certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { autoComplete = Nothing
      }

-- A list of all of the standard Xmonad prompts
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("p", passPrompt)        -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

------------------------------------------------------------------------
-- TREESELECT
------------------------------------------------------------------------
--
-- customizable menu for quick access, can be used for workspaces, programs, ssh, or virtualization.
-- add Nodes as seen below, to add more items to the menu
-- 
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "+ Virtualization" "VM Applications" (return ()))
       [ Node (TS.TSNode "Virt-Manager" "Virtual machine manager" (spawn "virt-manager")) []
       , Node (TS.TSNode "Virtualbox" "Oracle's virtualization program" (spawn "virtualbox")) []
       ]
    , Node (TS.TSNode "+ Terminals" "Terminal emulators" (return()))
       [ Node (TS.TSNode "Alacritty" "" (spawn "alacritty")) []
       , Node (TS.TSNode "Kitty" "" (spawn "kitty")) []
       , Node (TS.TSNode "ST" "" (spawn "st")) []
       , Node (TS.TSNode "URXVT" "" (spawn "urxvt")) []
       , Node (TS.TSNode "XTerm" "" (spawn "xterm")) []
       , Node (TS.TSNode "XFCE4-Terminal" "" (spawn "xfce4-terminal")) []
       ]
    , Node (TS.TSNode "+ Websites" "Website access" (return()))
       [ Node (TS.TSNode "SSH" "Server access" (spawn "st -e ssh root@jpedmedia.com")) []
       , Node (TS.TSNode "NotMyOwn" "update site" (spawn "st -e rsync -vrP --delete-after ~/notmyown/public/ root@jpedmedia.com:/var/www/notmyown")) []
       , Node (TS.TSNode "JPedMedia" "update site" (spawn "st -e rsync -vrP --delete-after ~/website/ root@jpedmedia.com/var/www/jpedmedia")) []
       ]     
    , Node (TS.TSNode "+ Games" "Lets Play a Game" (return ()))
       [ Node (TS.TSNode "+ Emulators" "Retrofun" (return ()))
          [ Node (TS.TSNode "PS3" "Playstation3 emulator" (spawn "rpcs3")) []
          , Node (TS.TSNode "PS2" "Playstation2 emulator" (spawn "PCSX2")) []
          , Node (TS.TSNode "Retroarch" "Old-School Fun" (spawn "retroarch")) []
          ]
       , Node (TS.TSNode "+ F.O.S.S" "Open source fun" (return ()))
          [ Node (TS.TSNode "SuperTuxKart" "Open source kart racing" (spawn "supertuxkart")) []
          , Node (TS.TSNode "Xonotic" "Fast-paced first person shooter" (spawn "xonotic")) []
          ]
       , Node (TS.TSNode "Steam" "The Steam gaming platform" (spawn "steam")) []
       ]
    , Node (TS.TSNode "+ Internet" "internet and web programs" (return ()))
       [ Node (TS.TSNode "Brave" "A privacy-oriented web browser" (spawn "brave")) []
       , Node (TS.TSNode "Firefox" "Open source web browser" (spawn "firefox")) []
       , Node (TS.TSNode "Qutebrowser" "Minimal web browser" (spawn "qutebrowser")) []
       , Node (TS.TSNode "Surf Browser" "Suckless surf web browser" (spawn "surf www.suckless.org")) []
       , Node (TS.TSNode "Badwolf" "Minimal browser" (spawn "badwolf")) []
       , Node (TS.TSNode "Nyxt" "lisp browser"  (spawn "nyxt")) []
       ]
    , Node (TS.TSNode "+ Office" "office applications" (return ()))
       [ Node (TS.TSNode "LibreOffice" "Open source office suite" (spawn "libreoffice")) []
       , Node (TS.TSNode "LibreOffice Base" "Desktop database front end" (spawn "lobase")) []
       , Node (TS.TSNode "LibreOffice Calc" "Spreadsheet program" (spawn "localc")) []
       , Node (TS.TSNode "LibreOffice Draw" "Diagrams and sketches" (spawn "lodraw")) []
       , Node (TS.TSNode "LibreOffice Impress" "Presentation program" (spawn "loimpress")) []
       , Node (TS.TSNode "LibreOffice Math" "Formula editor" (spawn "lomath")) []
       , Node (TS.TSNode "LibreOffice Writer" "Word processor" (spawn "lowriter")) []
       , Node (TS.TSNode "Calculator" "Gui version of qalc" (spawn "qalculate-gtk")) []
       ]  
    , Node (TS.TSNode "+ Programming" "Dev tools" (return ()))
       [ Node (TS.TSNode "Vim" "Text Editor" (spawn "st -e nvim")) []
       , Node (TS.TSNode "Notepadqq" "Plain Text Editor" (spawn "notepadqq")) []
       , Node (TS.TSNode "Nano" "Basic Text Editor" (spawn "st -e nano")) []
       ]
    , Node (TS.TSNode "+ Development" "Creation Software" (return ()))
       [ Node (TS.TSNode "OBS" "Video Creation Program" (spawn "obs")) []
       , Node (TS.TSNode "Simple Screen Recorder" "Screen Recorder" (spawn "simplescreenrecorder")) []
       , Node (TS.TSNode "Gimp" "Image Manipulator" (spawn "gimp")) []
       , Node (TS.TSNode "KDEnlive" "video editor" (spawn "kdenlive")) []
       , Node (TS.TSNode "Godot" "game engine" (spawn "godot")) []
       ]
    , Node (TS.TSNode "+ Network" "Network tools" (return  ()))
       [ Node (TS.TSNode "PacketTracer" "Cisco training tool" (spawn "packettracer")) []
       , Node (TS.TSNode "Wireshark" "Packet sniffer" (spawn "wireshark")) []
       ]
    , Node (TS.TSNode "+ System Configs" "System configuration files" (return ()))
        [ Node (TS.TSNode "+ Shell Configs" "My shell config files" (return ()))
            [ Node (TS.TSNode "Bash" "Borne again shell" (spawn "alacritty -e vim $HOME/.bashrc")) []
            , Node (TS.TSNode "Fish" "Friendly interactive shell" (spawn "alacritty -e vim $HOME/.config/fish/config.fish")) []
            , Node (TS.TSNode "Zsh" "Z-shell" (spawn "alacritty -e vim $HOME/.zshrc")) []
            ]
        , Node (TS.TSNode "+ Terminal" "Terminal configs" (return ()))
            [ Node (TS.TSNode "Alacritty" "Main terminal" (spawn "alacritty -e vim $HOME/.config/alacritty.yml")) []
            , Node (TS.TSNode "ST" "Simple terminal" (spawn "alacritty -e vim $HOME/dwm-6.2/st-0.8.4/config.def.h")) []
            , Node (TS.TSNode "URXVT" "rxvt-Unicode" (spawn "alacritty -e vim $HOME/.Xresources")) []
            , Node (TS.TSNode "XTerm" "Xterminal" (spawn "alacritty -e vim $HOME/.Xresources")) []
            ]
        ]
    ]
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd3b3b3b
                              --, TS.ts_background   = 0xdd222222
                              , TS.ts_font         = "xft:Roboto-14"
                              , TS.ts_node         = (0xffd8d8d8, 0xff000000)
                              , TS.ts_nodealt      = (0xffd8d8d8, 0xff3b3b3b)
                              --, TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_highlight    = (0xff000000, 0xff477D8F)
                              , TS.ts_extra        = 0xffd8d8d8
                              , TS.ts_node_width   = 220
                              , TS.ts_node_height  = 30
                              , TS.ts_originX      = 100
                              , TS.ts_originY      = 100
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    ]
------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
-- I am using the Xmonad.Util.EZConfig module which allows keybindings
-- to be written in simpler, emacs-like format.
myKeys :: [([Char], X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --restart")      
        , ("M-S-c", spawn "arcolinux-logout")

    -- Open my main terminal
        , ("M-S-<Return>", spawn "kitty")     
    
    -- Open my browsers
        , ("M-S-b", spawn "brave")                 
        , ("M-S-l", spawn "librewolf")             
        , ("M-C-q", spawn "qutebrowser")           
        , ("M-S-a", spawn "firefox")               
        , ("M-C-b", spawn "badwolf")               
        , ("M-S-n", spawn "nyxt")                  

    -- Wallpaper switcher
        , ("M-S-w", spawn "bgd")                   

    -- Run Prompts
        , ("M-C-<Return>", shellPrompt dtXPConfig)   
        , ("M-S-d", spawn "dmenu_run -p Execute: -g 10 -l 5 -nb '#3b3b3b' -fn 'Roboto' -nf '#000000' -sb '#000000' -sf '#477d8f'")    
        , ("M-S-e", spawn "./.config/.dmenu/edit_configs.sh")   
        , ("M-S-i", spawn "./.config/.dmenu/browsers.sh")
        , ("M-S-r", spawn "rofi -show drun")
        , ("M-S-t", treeselectAction tsDefaultConfig)
        , ("M-S-f", spawn "./.config/.dmenu/fm.sh")

    -- Windows
        , ("M-S-q", kill1)                           

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats"))       

    -- Scratchpads
        , ("M-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-n", namedScratchpadAction myScratchPads "ncmpcpp")
        , ("M-c", namedScratchpadAction myScratchPads "calculator")
        , ("M-m", namedScratchpadAction myScratchPads "mail")
        , ("M-r", namedScratchpadAction myScratchPads "ranger")
        , ("M-e", namedScratchpadAction myScratchPads "edit")
        , ("M-h", namedScratchpadAction myScratchPads "keys")

    -- Grid Select
        , ("M-t", spawnSelected' myAppGrid)                 
        , ("M-g", goToSelected $ mygridConfig myColorizer)  
        , ("M-b", bringSelected $ mygridConfig myColorizer) 

    -- Windows navigation
        , ("M-C-m", windows W.focusMaster)     
        , ("M-<Left>", windows W.focusDown)    
        , ("M-<Right>", windows W.focusUp)     
        , ("M-<Up>", windows W.swapMaster)    
        , ("M-S-<Left>", windows W.swapDown)   
        , ("M-S-<Right>", windows W.swapUp)    
        , ("M-S-<Down>", sendMessage Shrink)   
        , ("M-S-<Up>", sendMessage Expand)     

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                                    
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) 
        , ("M-S-<Space>", sendMessage ToggleStruts)                              
        , ("M-C-n", sendMessage $ MT.Toggle NOBORDERS)                           

    -- Multimedia Keys
        , ("<Print>", spawn "flameshot gui")
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -10%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +10%")
        , ("M-S-s", spawn "playerctl stop")
        ]
          
    -- Appending named scratchpads to keybindings list
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))
                
------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- My workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool.

xmobarEscape :: [Char] -> [Char]
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape) 
               $ [ " \xf1c0  ",  " \xf269  ",  " \xf19d  ",  " \xf121  ",  " \xf07c  ",  " \xf03d  ",  " \xf15c  ",  " \xf120  ",  " \xf0e0  "]
               -- $ [ "Web", "Edu", "Dev", "Dir", "Media", "Doc", "Mail", "Git"]
               -- $ [ " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9"]
  where                                                                      
        clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,                                        
                      let n = i ] 

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title 
-- of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out 
     -- the full name of my clickable workspaces, which would look like:
     -- doShift "<action xdotool super+8>gfx</action>"
     [ className =? "obs"                  --> doShift ( myWorkspaces !! 8)
     , title =? "Kdenlive"                 --> doShift ( myWorkspaces !! 8)
     , className =? "SimpleScreenRecorder" --> doShift ( myWorkspaces !! 8)
--     , className =? "firefox"              --> doShift ( myWorkspaces !! 1)
     , title =? "LibreOffice"              --> doShift ( myWorkspaces !! 5)
--     , className =? "Brave-browser"        --> doShift ( myWorkspaces !! 1)
--     , className =? "qutebrowser"          --> doShift ( myWorkspaces !! 1)
--     , className =? "LibreWolf"            --> doShift ( myWorkspaces !! 1)
--     , className =? "Nyxt"                 --> doShift ( myWorkspaces !! 1)  
     ] <+> namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- LOGHOOK
------------------------------------------------------------------------
-- Sets opacity for inactive (unfocused) windows. I prefer to not use
-- this feature so I've set opacity to 1.0. If you want opacity, set
-- this to a value of less than 1 (such as 0.9 for 90% opacity).
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.9

------------------------------------------------------------------------
-- LAYOUTS
------------------------------------------------------------------------
-- Makes setting the spacingRaw simpler to write. The spacingRaw
-- module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
accordion= renamed [Replace "accordion"]
           $ limitWindows 12
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 1
           $ ResizableTall 1 (3/100) (3/5) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 2
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20
           $ Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20
           $ simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 2
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing' 4
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Roboto Font:regular:pixelsize=11"
                      , activeColor         = "#292d3e"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }
          
-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| noBorders tabs
                                 ||| spirals
                                 ||| threeCol
                                 ||| threeRow
                                 ||| Accordion  
------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
    -- Launching xmobar
    xmproc0 <- spawnPipe "xmobar -x 0 /home/jake/.config/xmobar/xmobarrc"
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
        -- Run xmonad commands from command line with "xmonadctl command". Commands include:
        -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
        -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run 
        -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
        , handleEventHook    = serverModeEventHookCmd 
                               <+> serverModeEventHook 
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook 
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x                -- >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
                        , ppCurrent = xmobarColor "#477d8f" "" . wrap "*" "" -- Current workspace in xmobar
                        , ppVisible = xmobarColor "#3b3b3b" ""                -- Visible but not current workspace
                        , ppHidden = xmobarColor "#b8b8b8" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#3b3b3b" ""        -- Hidden workspaces (no windows)
                        , ppTitle = xmobarColor "#875f87" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep =  "<fc=#477d8f>     </fc>"                     -- Separators in xmobar
                        , ppUrgent = xmobarColor "#A12B2B" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras  = [windowCount]                           -- # of windows current workspace
                        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        } `additionalKeysP` myKeys
