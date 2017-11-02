{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Garlic.View
(
    -- * Application Window
    GarlicApp,
    appHeader,
    appVRecipes,
    appVTracking,
    appAppMenu,
    appIngredientEd,
    appEnableSearch,
    appReplaceIngr,
    appDisplayError,
    appSearchChange,
    appActivate,
    appShutdown,
    appStartup,
    appQuit,
    appAbout,
    application,

    -- * App Menu
    GarlicAppMenu,
    amIngEditor,
    amIngImport,
    amAbout,
    amQuit,
)
where

import Control.Monad
import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Garlic.Types
import Reactive.Banana.Frameworks (mapEventIO)
import Reactive.Banana (filterJust)
import Reactive.Banana.GI.Gtk

import Garlic.View.HeaderBar
import Garlic.View.IngredientEditor
import Garlic.View.Recipe
import Garlic.View.Tracking

import qualified GI.Gio as Gio

uiMainWindow :: Text
uiMainWindow = decodeUtf8 $(embedFile "res/main-window.ui")

uiAboutDialog :: Text
uiAboutDialog = decodeUtf8 $(embedFile "res/about-dialog.ui")

data GarlicApp = GarlicApp
    { _appHeader        :: GarlicHeader
    , _appVRecipes      :: GarlicViewRecipes
    , _appVTracking     :: GarlicViewTracking
    , _appAppMenu       :: GarlicAppMenu
    , _appIngredientEd  :: GarlicIngredientEditor
    , _appEnableSearch  :: Consumer ()
    , _appDisplayError  :: Consumer Text
    , _appReplaceIngr   :: Consumer [Text]
    , _appSearchChange  :: Event Text
    , _appActivate      :: Event ()
    , _appStartup       :: Event ()
    , _appShutdown      :: Event ()
    , _appQuit          :: Consumer ()
    , _appAbout         :: Consumer ()
    }

application :: Application -> Garlic GarlicApp
application app = do
    b <- builderNew
    _ <- builderAddFromString b uiMainWindow (-1)

    -- Widgets
    win         <- castB b "applicationWindow" ApplicationWindow
    searchBar   <- castB b "searchBar" SearchBar
    searchEntry <- castB b "searchEntry" SearchEntry
    infoBar     <- castB b "infoBar" InfoBar
    infoLabel   <- castB b "infoLabel" Label
    mainStack   <- castB b "mainStack" Stack

    -- Ingredient Completion
    (newCompl, replaceCompl) <- ingredientCompletion

    -- Views
    vRecipes  <- viewRecipes newCompl mainStack
    vTracking <- viewTracking newCompl mainStack

    -- Sub elements
    hb   <- headerBar win mainStack

    -- App Menu
    menu <- appMenu app win
    _ <- on app #startup $ applicationSetAppMenu app (Just (amMenu menu))

    -- Ingredient Editor
    editor <- ingredientEditor win newCompl

    -- Hardcoded window setting on activation
    _ <- on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    -- Hardcode info hide
    _ <- on infoBar #close $ set infoBar [ #visible := False ]
    _ <- on infoBar #response $ \_ -> set infoBar [ #visible := False ]

    lift $ GarlicApp 
       <$> pure hb                          -- HeaderBar
       <*> pure vRecipes
       <*> pure vTracking
       <*> pure menu
       <*> pure editor
       <*> pure (toggleSearch searchBar)    -- Search Toggle
       <*> pure (ioConsumer $ \t -> do
               set infoBar [ #visible := True ]
               set infoLabel [ #label := t ])
       <*> pure replaceCompl
       <*> (mapEventIO (\_ -> get searchEntry #text) 
                =<< signalE0 searchEntry #searchChanged)
       <*> signalE0 app #activate
       <*> signalE0 app #startup
       <*> signalE0 app #shutdown
       <*> pure (ioConsumer $ \_ -> widgetDestroy win)
       <*> pure (ioConsumer $ \_ -> about win)

about :: MonadIO m => ApplicationWindow -> m ()
about appWin = do
    b <- builderNew
    _ <- builderAddFromString b uiAboutDialog (-1)

    dialog <- castB b "aboutDialog" AboutDialog
    windowSetTransientFor dialog (Just appWin)

    _ <- on dialog #deleteEvent $ \_ -> widgetDestroy dialog >> pure True

    void $ dialogRun dialog

data GarlicAppMenu = GarlicAppMenu
    { _amIngEditor :: Event ()
    , _amIngImport :: Event FilePath
    , _amAbout     :: Event ()
    , _amQuit      :: Event ()
    , amMenu       :: Gio.Menu
    }

appMenu :: Application -> ApplicationWindow -> Garlic GarlicAppMenu
appMenu app win = do
    menu <- new Gio.Menu []

    let append m = mapM_ (\(a,b) -> (Gio.menuAppend m (Just a) (Just b)))

    ingMenu <- new Gio.Menu []
    append ingMenu
        [ ("Editor", "app.ingredients")
        , ("Import CSV", "app.csv")
        ]

    Gio.menuAppendSection menu (Just "Ingredients") ingMenu

    generalMenu <- new Gio.Menu []

    append generalMenu 
        [ ("About", "app.about")
        , ("Quit", "app.quit") ]

    Gio.menuAppendSection menu Nothing generalMenu

    GarlicAppMenu
        <$> menuClick "ingredients"
        <*> (importIngredients win =<< menuClick "csv")
        <*> menuClick "about"
        <*> menuClick "quit"
        <*> pure menu

    where menuClick s = do
              action <- Gio.simpleActionNew s Nothing
              Gio.simpleActionSetEnabled action True
              Gio.actionMapAddAction app action
              lift $ signalEN action #activate (\h _ -> h ())

-- | Toggle a 'SearchBar'
toggleSearch :: SearchBar -> Consumer ()
toggleSearch s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

importIngredients :: ApplicationWindow -> Event () -> Garlic (Event FilePath)
importIngredients win = lift . fmap filterJust . mapEventIO (const go)
    where go = do
              fc <- new FileChooserDialog []
              _  <- dialogAddButton fc "Import" 0
              _  <- dialogAddButton fc "Cancel" 1
              dialogSetDefaultResponse fc 0
              windowSetTransientFor fc (Just win)

              r <- dialogRun fc

              case r of
                  0 -> do
                       path <- fileChooserGetFilename fc
                       widgetDestroy fc
                       pure path
                  1 -> widgetDestroy fc >> pure Nothing
                  _ -> error "importIngredients: Invalid response"

ingredientCompletion 
    :: MonadIO m 
    => Garlic (m EntryCompletion, Consumer [Text])
ingredientCompletion = do
    model <- listStoreNew [gtypeString]

    let newCompl = do
            compl <- new EntryCompletion 
                [ #model := model
                , #textColumn := 0 ]

            trender <- new CellRendererText []
            Just area <- get compl #cellArea
            cellLayoutPackStart area trender True
            cellLayoutAddAttribute area trender "text" 0

            pure compl

    let cons = ioConsumer $ \xs -> do
            listStoreClear model
            mapM_ (appendOne model) xs

    return (newCompl, cons)

appendOne :: MonadIO m => ListStore -> Text -> m ()
appendOne model str = do
    x <- liftIO $ toGValue (Just str)
    i <- listStoreAppend model
    listStoreSet model i [0] [x]

-- LENSES --
makeGetters ''GarlicApp
makeGetters ''GarlicAppMenu
