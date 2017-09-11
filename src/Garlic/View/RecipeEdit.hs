{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Garlic.View.RecipeEdit
(
    GarlicRecipeEdit,
    showEditor,
    editSetInstructions,
    editInstructions,
    editMasks,
    editDelete,
    editStore,
    editIngredients,
    editNewIngredient,
    editEnterIngredient,
    recipeEdit,

    GarlicRecipeEditMask,
    editSetName,
    editSetCuisine,
    editSetDuration,
    editSetYield,
    editSetYieldUnit,
    editSetSource,
    editSetURL,
    editSetRating,
    editName,
    editCuisine,
    editDuration,
    editYield,
    editYieldUnit,
    editSource,
    editURL,
    editRating,

    GarlicNewIngredient,
    niClearClick,
    niOkClick,
    niMask,

    GarlicIngredientList,
    ilInserted,
    ilChanged,
    ilDeleted,
    ilAppend,
    ilClear,

    EditorIngredient(..)
)
where

import Control.Monad.Trans
import Control.Monad
import Data.FileEmbed
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk hiding (Unit)
import GI.GtkSource
import Garlic.Types
import Garlic.View.IngredientEditor
import Garlic.Data.Units
import Reactive.Banana.GI.Gtk
import Reactive.Banana (stepper)
import Reactive.Banana.Frameworks (mapEventIO, MomentIO, reactimate)
import Text.Printf
import Text.Markdown (Markdown (..))

uiRecipeEdit :: Text
uiRecipeEdit = decodeUtf8 $(embedFile "res/recipe-edit.ui")

uiIngredientNew :: Text
uiIngredientNew = decodeUtf8 $(embedFile "res/ingredient-new.ui")

data GarlicRecipeEdit = GarlicRecipeEdit
    { _showEditor          :: Consumer ()
    , _editSetInstructions :: Consumer Markdown
    , _editInstructions    :: Behavior (Maybe Markdown)
    , _editMasks           :: GarlicRecipeEditMask
    , _editNewIngredient   :: GarlicNewIngredient
    , _editIngredients     :: GarlicIngredientList
    , _editEnterIngredient :: Event Text
    , _editDelete          :: Event ()
    , _editStore           :: Event ()
    }

recipeEdit :: Stack -> Garlic EntryCompletion -> Garlic GarlicRecipeEdit
recipeEdit stack newCompl = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeEdit (-1)

    redt <- castB b "recipeEditBox" Box
    stackAddNamed stack redt "recipeEdit"

    -- Add SourceView manually
    (sourceview, sbuf) <- buildSourceView
    sourcevp <- castB b "instructionVP" Viewport
    setContainerChild sourcevp sourceview

    masks <- getEditMasks b
    (deleteButton, storeButton) <- actionButtons b

    -- New Ingredient Popover
    newButton <- castB b "ingredientNew" MenuButton
    popover <- newIngredient newButton

    -- Ingredient List
    ingredientSearch  <- castB b "ingredientSearch" Entry
    ingredientTree    <- castB b "ingredientTree" TreeView
    ingredientStore   <- castB b "ingredientStore" ListStore
    ingredientDelete  <- castB b "ingredientDelete" Button

    newCompl >>= \compl -> set ingredientSearch [ #completion := compl ]

    treeSelection <- treeViewGetSelection ingredientTree
    set treeSelection [ #mode := SelectionModeMultiple ]

    inglist <- ingredientList ingredientTree ingredientStore ingredientDelete

    lift $ GarlicRecipeEdit
       <$> pure (ioConsumer (\_ -> stackSetVisibleChild stack redt))
       <*> pure (ioConsumer (\(Markdown t) -> set sbuf [ #text := toStrict t ]))
       <*> (fmap (fmap (Markdown . fromStrict)) <$> attrB sbuf #text)
       <*> pure masks
       <*> pure popover
       <*> pure inglist
       <*> ingredientEnter ingredientSearch
       <*> signalE0 deleteButton #clicked
       <*> signalE0 storeButton #clicked

ingredientEnter :: Entry -> MomentIO (Event Text)
ingredientEnter e = do
    enter <- mapEventIO go =<< signalE0 e #activate
    reactimate $ entrySetText e "" <$ enter
    pure enter
    where go _ = entryGetText e

actionButtons :: MonadIO m => Builder -> m (Button, Button)
actionButtons b = do
    actionBar    <- castB b "actionBar" ActionBar
    deleteButton <- new Button [ #label := "Delete" ]
    storeButton  <- new Button [ #label := "Store" ]

    deleteContext <- widgetGetStyleContext deleteButton
    styleContextAddClass deleteContext STYLE_CLASS_DESTRUCTIVE_ACTION

    storeContext <- widgetGetStyleContext storeButton
    styleContextAddClass storeContext STYLE_CLASS_SUGGESTED_ACTION

    actionBarPackStart actionBar deleteButton
    actionBarPackEnd actionBar storeButton

    pure (deleteButton, storeButton)

buildSourceView :: MonadIO m => m (View, Buffer)
buildSourceView = do
    lm <- languageManagerGetDefault
    markdown <- languageManagerGetLanguage lm "markdown"
    sbuf <- new Buffer $ case markdown of
                Just md -> [ #language := md ]
                Nothing -> []
    sourceview <- viewNewWithBuffer sbuf
    set sourceview [ #smartBackspace := True
                   , #monospace := True
                   , #wrapMode := WrapModeWord
                   ]

    pure (sourceview, sbuf)

appendOne :: MonadIO m => ListStore -> Text -> m ()
appendOne model str = do
    x <- liftIO $ toGValue (Just str)
    i <- listStoreAppend model
    listStoreSet model i [0] [x]

data GarlicRecipeEditMask = GarlicRecipeEditMask
    { _editSetName      :: Consumer Text
    , _editSetCuisine   :: Consumer Text
    , _editSetDuration  :: Consumer Text
    , _editSetYield     :: Consumer Double
    , _editSetYieldUnit :: Consumer Text
    , _editSetSource    :: Consumer Text
    , _editSetURL       :: Consumer Text
    , _editSetRating    :: Consumer Int
    , _editName         :: Behavior Text
    , _editCuisine      :: Behavior Text
    , _editDuration     :: Behavior Text
    , _editYield        :: Behavior Double
    , _editYieldUnit    :: Behavior Text
    , _editSource       :: Behavior Text
    , _editURL          :: Behavior Text
    , _editRating       :: Behavior Int
    }

getEditMasks :: Builder -> Garlic GarlicRecipeEditMask
getEditMasks b = do
    recipeName      <- castB b "recipeName" Entry
    recipeCuisine   <- castB b "recipeCuisine" Entry
    recipeDuration  <- castB b "recipeDuration" Entry
    recipeYieldUnit <- castB b "recipeYieldUnit" Entry
    recipeSource    <- castB b "recipeSource" Entry
    recipeURL       <- castB b "recipeURL" Entry
    recipeYield     <- castB b "recipeYield" SpinButton
    recipeRating    <- castB b "recipeRating" SpinButton

    lift $ GarlicRecipeEditMask
       <$> pure (ioConsumer $ entrySetText recipeName)
       <*> pure (ioConsumer $ entrySetText recipeCuisine)
       <*> pure (ioConsumer $ entrySetText recipeDuration)
       <*> pure (ioConsumer $ spinButtonSetValue recipeYield)
       <*> pure (ioConsumer $ entrySetText recipeYieldUnit)
       <*> pure (ioConsumer $ entrySetText recipeSource)
       <*> pure (ioConsumer $ entrySetText recipeURL)
       <*> pure (ioConsumer $ spinButtonSetValue recipeRating . fromIntegral)
       <*> attrB recipeName #text
       <*> attrB recipeCuisine #text
       <*> attrB recipeDuration #text
       <*> attrB recipeYield #value 
       <*> attrB recipeYieldUnit #text
       <*> attrB recipeSource #text
       <*> attrB recipeURL #text
       <*> (fmap truncate <$> attrB recipeRating #value)

data GarlicNewIngredient = GarlicNewIngredient
    { _niClearClick  :: Event ()
    , _niOkClick     :: Event ()
    , _niMask        :: GarlicIngredientMask
    }

newIngredient :: MenuButton -> Garlic GarlicNewIngredient
newIngredient button = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientNew (-1)

    clearButton <- castB b "clearButton" Button
    okButton    <- castB b "okButton" Button
    box         <- castB b "box" Box
    popover     <- castB b "ingredientNew" Popover

    mask <- ingredientMask box
    menuButtonSetPopover button (Just popover)

    GarlicNewIngredient
        <$> lift (signalE0 clearButton #clicked)
        <*> lift (signalE0 okButton #clicked)
        <*> pure mask

data EditorIngredient = EditorIngredient
    { eiAmount   :: Double
    , eiUnit     :: Unit
    , eiName     :: Text
    , eiDisplay  :: Maybe Text
    , eiOptional :: Bool
    }
    deriving (Eq, Show)

data GarlicIngredientList = GarlicIngredientList
    { _ilInserted :: Event (Int, EditorIngredient)
    , _ilChanged  :: Event (Int, EditorIngredient)
    , _ilDeleted  :: Event Int
    , _ilAppend   :: Consumer [EditorIngredient]
    , _ilClear    :: Consumer ()
    }

ingredientList :: TreeView -> ListStore -> Button -> Garlic GarlicIngredientList
ingredientList view model delButton = do
    units <- unitStore

    -- Deletion to model
    _ <- on delButton #clicked $ do
        sel <- treeViewGetSelection view
        (rows, _) <- treeSelectionGetSelectedRows sel
        forM_ rows $ \row -> do
            (b, iter)  <- treeModelGetIter model row
            when b . void $ listStoreRemove model iter

    -- Build new Cell Renderers
    amntR <- new CellRendererText [ #editable := True ]
    _ <- on amntR #edited $ \path txt -> do
        path' <- treePathNewFromString path
        (b, iter)  <- treeModelGetIter model path'
        when (b && parseNum @Double txt /= 0) $ do
            txt' <- toGValue (Just txt)
            listStoreSetValue model iter 0 txt'

    unitR <- new CellRendererCombo  [ #editable    := True
                                    , #hasEntry    := False
                                    , #model       := units
                                    , #textColumn  := 0 ]
    _ <- on unitR #edited $ \path txt -> do
        path' <- treePathNewFromString path
        (b, iter) <- treeModelGetIter model path'
        when b $ do
            txt' <- toGValue (Just txt)
            listStoreSetValue model iter 1 txt'

    nameR <- new CellRendererText   []
    dispR <- new CellRendererText   [ #editable := True ]
    _ <- on dispR #edited $ \path txt -> do
        path' <- treePathNewFromString path
        (b, iter) <- treeModelGetIter model path'
        when b $ do
            txt' <- toGValue (Just txt)
            listStoreSetValue model iter 3 txt'

    optiR <- new CellRendererToggle [ #activatable := True ]
    _ <- on optiR #toggled $ \path -> do
        path' <- treePathNewFromString path
        (b, iter) <- treeModelGetIter model path'
        when b $ do
            x <- treeModelGetValue model iter 4
            x' <- toGValue . not =<< fromGValue x
            listStoreSetValue model iter 4 x'

    -- Columns
    amntC <- new TreeViewColumn [ #title := "Amount", #resizable := True]
    treeViewColumnPackStart amntC amntR False
    treeViewColumnAddAttribute amntC amntR "text" 0
    unitC <- new TreeViewColumn [ #title := "Unit", #resizable := True]
    treeViewColumnPackStart unitC unitR False
    treeViewColumnAddAttribute unitC unitR "text" 1
    nameC <- new TreeViewColumn [ #title := "Name", #resizable := True ]
    treeViewColumnPackStart nameC nameR True
    treeViewColumnAddAttribute nameC nameR "text" 2
    dispC <- new TreeViewColumn [ #title := "Display Name", #resizable := True ]
    treeViewColumnPackStart dispC dispR True
    treeViewColumnAddAttribute dispC dispR "text" 3
    optiC <- new TreeViewColumn [ #title := "Optional" ]
    treeViewColumnPackStart optiC optiR False
    treeViewColumnAddAttribute optiC optiR "active" 4

    mapM_ (treeViewAppendColumn view) [ amntC, unitC, nameC, dispC, optiC ]

    lift $ GarlicIngredientList 
       <$> signalEN model #rowInserted (\h p i -> fetch p i >>= h)
       <*> signalEN model #rowChanged (\h p i -> fetch p i >>= h)
       <*> signalEN model #rowDeleted 
               (\h p -> treePathToString p >>= h . parseNum)
       <*> pure (ioConsumer (mapM_ append))
       <*> pure (ioConsumer (\_ -> listStoreClear model))

    where append :: MonadIO m => EditorIngredient -> m ()
          append EditorIngredient{..} = do
              let astr = printf "%.2f" eiAmount :: String
                  utxt = prettyUnit eiUnit :: Text

              amnt <- liftIO $ toGValue . Just $ astr
              unit <- liftIO $ toGValue . Just $ utxt
              name <- liftIO $ toGValue . Just $ eiName
              disp <- liftIO $ toGValue eiDisplay
              opt  <- liftIO $ toGValue eiOptional

              i  <- listStoreAppend model
              listStoreSet model i [0..4] [amnt, unit, name, disp, opt]

          fetch :: MonadIO m => TreePath -> TreeIter 
                -> m (Int, EditorIngredient)
          fetch p i = do
              idx         <- parseNum <$> treePathToString p
              [a,b,c,d,e] <- mapM (treeModelGetValue model i) [0..4]
              x <- liftIO $ EditorIngredient 
               <$> (maybe 0 parseNum <$> fromGValue a)
               <*> (maybe Gram (parseUnit @Text) <$> fromGValue b)
               <*> (fromMaybe "" <$> fromGValue c) 
               <*> fromGValue d
               <*> fromGValue e
              pure (idx,x)

unitStore :: MonadIO m => m ListStore
unitStore = do
    units <- listStoreNew [ gtypeString ]
    mapM_ (appendOne units . prettyUnit) allUnits
    return units

-- LENSES
makeGetters ''GarlicRecipeEdit
makeGetters ''GarlicRecipeEditMask
makeGetters ''GarlicNewIngredient
makeGetters ''GarlicIngredientList
