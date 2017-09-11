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
    appRecipeEdit,
    appRecipeDisplay,
    appRecipeList,
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

    -- * Recipe List
    GarlicRecipes,
    addRecipes,
    clearRecipes,
    recipeSelected,

    ListRecipe (..),
    lrRating,
    lrDuration,
    lrName,
    lrCuisine,
)
where

import Control.Lens.TH
import Control.Monad
import Control.Monad.Trans
import Data.FileEmbed
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Garlic.Types
import Reactive.Banana.Frameworks (mapEventIO)
import Reactive.Banana (filterJust)
import Reactive.Banana.GI.Gtk
import Text.Printf

import Garlic.Data.Duration
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay
import Garlic.View.RecipeEdit
import Garlic.View.IngredientEditor

import qualified GI.Gio as Gio

uiMainWindow :: Text
uiMainWindow = decodeUtf8 $(embedFile "res/main-window.ui")

uiRecipeEntry :: Text
uiRecipeEntry = decodeUtf8 $(embedFile "res/recipe-entry.ui")

uiAboutDialog :: Text
uiAboutDialog = decodeUtf8 $(embedFile "res/about-dialog.ui")

data GarlicApp = GarlicApp
    { _appHeader        :: GarlicHeader
    , _appRecipeDisplay :: GarlicRecipeDisplay
    , _appRecipeEdit    :: GarlicRecipeEdit
    , _appRecipeList    :: GarlicRecipes
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
    rlist       <- castB b "recipeList" ListBox
    rstack      <- castB b "recipeStack" Stack
    searchBar   <- castB b "searchBar" SearchBar
    searchEntry <- castB b "searchEntry" SearchEntry
    infoBar     <- castB b "infoBar" InfoBar
    infoLabel   <- castB b "infoLabel" Label

    -- Ingredient Completion
    (newCompl, replaceCompl) <- ingredientCompletion

    -- Sub elements
    hb   <- headerBar win
    rdis <- recipeDisplay rstack
    redt <- recipeEdit rstack newCompl
    recs <- recipes rlist

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
       <*> pure rdis                        -- Recipe Display
       <*> pure redt                        -- Recipe Editor
       <*> pure recs                        -- Recipe List
       <*> pure menu
       <*> pure editor
       <*> pure (searchToggle searchBar)    -- Search Toggle
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
searchToggle :: SearchBar -> Consumer ()
searchToggle s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

-- | Used by 'addRecipes' in 'GarlicRecipes' to add new recipes to the list
data ListRecipe = ListRecipe 
    { _lrRating   :: Int
    , _lrDuration :: Int
    , _lrName     :: Text
    , _lrCuisine  :: Text
    }

data GarlicRecipes = GarlicRecipes
    { _clearRecipes   :: Consumer ()
    , _addRecipes     :: Consumer (Seq ListRecipe)
    , _recipeSelected :: Event Int
    }

recipes :: ListBox -> Garlic GarlicRecipes
recipes rlist = 
    lift $ GarlicRecipes
       <$> pure (ioConsumer $ \_ -> clearList rlist)
       <*> pure (ioConsumer $ mapM_ append)
       <*> (mapEventIO (fmap fromIntegral . listBoxRowGetIndex) 
                =<< signalE1 rlist #rowActivated)
    
    where clearList l = 
              containerGetChildren l >>= mapM_ (containerRemove l)
          append (ListRecipe a b d e) = 
              recipeEntry a b d e >>= \x -> listBoxInsert rlist x (-1)

-- | Build a new ListBoxRow for a recipe entry in the recipe sidebar/list.
recipeEntry 
    :: MonadIO m 
    => Int          -- ^ Rating
    -> Int          -- ^ Time Required
    -> Text         -- ^ Name
    -> Text         -- ^ Cuisine
    -> m ListBoxRow
recipeEntry rate time name cuisine = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeEntry (-1)
    
    nameL <- castB b "recipeName" Label
    infoL <- castB b "recipeInfo" Label

    let info  = pack $ printf "%s —  %s — %s" rating cuisine time'

    set nameL [ #label := name ]
    set infoL [ #label := info ]

    castB b "recipeEntry" ListBoxRow

    where rating = ratingString rate
          time'  = durationString time 

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
makeLenses ''ListRecipe
makeGetters ''GarlicApp
makeGetters ''GarlicRecipes
makeGetters ''GarlicAppMenu
