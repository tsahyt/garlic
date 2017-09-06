{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Garlic.View.RecipeEdit
(
    GarlicRecipeEdit,
    showEditor,
    editSetInstructions,
    editInstructions,
    editMasks,
    editDelete,
    editStore,
    editAbort,
    editNewIngredient,
    editEnterIngredient,
    editReplaceIngCompl,
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
    niClearAll,
    niClearClick,
    niOkClick,
    niName,
    niComment,
    niAmount,
    niUnit,
    niProtein,
    niCarbs,
    niSugar,
    niFibre,
    niFat,
    niSatFat,
    niPolyFat,
    niMonoFat,
    niTransFat,
)
where

import Control.Monad
import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk hiding (Unit)
import GI.GtkSource
import Garlic.Types
import Garlic.Data.Units
import Reactive.Banana.GI.Gtk
import Reactive.Banana (stepper, never)
import Reactive.Banana.Frameworks (mapEventIO, MomentIO, reactimate)
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
    , _editReplaceIngCompl :: Consumer [Text]
    , _editDelete          :: Event ()
    , _editAbort           :: Event ()
    , _editStore           :: Event ()
    }

recipeEdit :: Stack -> Garlic GarlicRecipeEdit
recipeEdit stack = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeEdit (-1)

    redt <- castB b "recipeEditBox" Box
    stackAddNamed stack redt "recipeEdit"

    -- Add SourceView manually
    (sourceview, sbuf) <- buildSourceView
    sourcevp <- castB b "instructionVP" Viewport
    setContainerChild sourcevp sourceview

    masks <- getEditMasks b
    (deleteButton, abortButton, storeButton) <- actionButtons b

    -- New Ingredient Popover
    newButton <- castB b "ingredientNew" MenuButton
    popover <- newIngredient newButton

    -- Ingredient List
    ingredientSearch  <- castB b "ingredientSearch" Entry
    replaceCompletion <- ingredientCompletion ingredientSearch
    ingredientTree    <- castB b "ingredientTree" TreeView
    ingredientStore   <- castB b "ingredientStore" TreeStore

    inglist <- ingredientList ingredientTree ingredientStore

    lift $ GarlicRecipeEdit
       <$> pure (ioConsumer (\_ -> stackSetVisibleChild stack redt))
       <*> pure (ioConsumer (\(Markdown t) -> set sbuf [ #text := toStrict t ]))
       <*> (fmap (fmap (Markdown . fromStrict)) <$> attrB sbuf #text)
       <*> pure masks
       <*> pure popover
       <*> pure inglist
       <*> ingredientEnter ingredientSearch
       <*> pure replaceCompletion
       <*> signalE0 deleteButton #clicked
       <*> signalE0 abortButton #clicked
       <*> signalE0 storeButton #clicked

ingredientEnter :: Entry -> MomentIO (Event Text)
ingredientEnter e = do
    enter <- mapEventIO go =<< signalE0 e #activate
    reactimate $ entrySetText e "" <$ enter
    pure enter
    where go _ = entryGetText e

actionButtons :: MonadIO m => Builder -> m (Button, Button, Button)
actionButtons b = do
    actionBar    <- castB b "actionBar" ActionBar
    deleteButton <- new Button [ #label := "Delete" ]
    abortButton  <- new Button [ #label := "Abort" ]
    storeButton  <- new Button [ #label := "Store" ]

    deleteContext <- widgetGetStyleContext deleteButton
    styleContextAddClass deleteContext STYLE_CLASS_DESTRUCTIVE_ACTION

    storeContext <- widgetGetStyleContext storeButton
    styleContextAddClass storeContext STYLE_CLASS_SUGGESTED_ACTION

    actionBarPackStart actionBar deleteButton
    actionBarPackStart actionBar abortButton
    actionBarPackEnd actionBar storeButton

    pure (deleteButton, abortButton, storeButton)

buildSourceView :: MonadIO m => m (View, Buffer)
buildSourceView = do
    lm <- languageManagerGetDefault
    markdown <- languageManagerGetLanguage lm "markdown"
    sbuf <- new Buffer $ case markdown of
                Just md -> [ #language := md ]
                Nothing -> []
    sourceview <- viewNewWithBuffer sbuf
    set sourceview [ #showLineNumbers := True
                   , #smartBackspace := True
                   , #monospace := True
                   ]

    pure (sourceview, sbuf)

ingredientCompletion :: Entry -> Garlic (Consumer [Text])
ingredientCompletion entry = do
    model <- listStoreNew [gtypeString]

    compl <- new EntryCompletion 
        [ #model := model
        , #textColumn := 0 ]

    trender <- new CellRendererText []
    Just area <- get compl #cellArea
    cellLayoutPackStart area trender True
    cellLayoutAddAttribute area trender "text" 0
    
    set entry [ #completion := compl ]

    return $ ioConsumer $ \xs -> 
        listStoreClear model >> mapM_ (appendOne model) xs

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
    { _niClearAll   :: Consumer ()
    , _niClearClick :: Event ()
    , _niOkClick    :: Event ()
    , _niName       :: Behavior Text
    , _niComment    :: Behavior Text
    , _niAmount     :: Behavior Text
    , _niUnit       :: Behavior Unit
    , _niProtein    :: Behavior Text
    , _niCarbs      :: Behavior Text
    , _niSugar      :: Behavior Text
    , _niFibre      :: Behavior Text
    , _niFat        :: Behavior Text
    , _niSatFat     :: Behavior Text
    , _niPolyFat    :: Behavior Text
    , _niMonoFat    :: Behavior Text
    , _niTransFat   :: Behavior Text
    }

newIngredient :: MenuButton -> Garlic GarlicNewIngredient
newIngredient button = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientNew (-1)

    popover <- castB b "ingredientNew" Popover
    set button [ #popover := popover ]

    name     <- castB b "niName" Entry
    comment  <- castB b "niComment" Entry
    unit     <- castB b "niUnit" ComboBoxText
    amount   <- castB b "niAmount" Entry
    protein  <- castB b "niProtein" Entry
    carbs    <- castB b "niCarbs" Entry
    sugar    <- castB b "niSugar" Entry
    fibre    <- castB b "niFibre" Entry
    fat      <- castB b "niFat" Entry
    satFat   <- castB b "niSatFat" Entry
    polyFat  <- castB b "niPolyFat" Entry
    monoFat  <- castB b "niMonoFat" Entry
    transFat <- castB b "niTransFat" Entry

    mapM_ (comboBoxTextAppendText unit . prettyUnit) allUnits

    let clearAll = mapM_ (flip setEntryText "")
            [ name, comment, amount, protein, carbs, sugar, fibre
            , fat, satFat, polyFat, monoFat, transFat ]

    okButton    <- castB b "okButton" Button
    clearButton <- castB b "clearButton" Button

    lift $ GarlicNewIngredient
       <$> pure (ioConsumer $ \_ -> clearAll)
       <*> signalE0 clearButton #clicked
       <*> signalE0 okButton #clicked
       <*> attrB name #text
       <*> attrB comment #text
       <*> attrB amount #text
       <*> comboBoxUnitB unit
       <*> attrB protein #text
       <*> attrB carbs #text
       <*> attrB sugar #text
       <*> attrB fibre #text
       <*> attrB fat #text
       <*> attrB satFat #text
       <*> attrB polyFat #text
       <*> attrB monoFat #text
       <*> attrB transFat #text

comboBoxUnitB :: ComboBoxText -> MomentIO (Behavior Unit)
comboBoxUnitB box = do
    c  <- signalE0 box #changed
    c' <- mapEventIO (const $ comboBoxTextGetActiveText box) c
    stepper Gram $ parseUnit <$> c'

data GarlicIngredientList = GarlicIngredientList
    { _ilChanged :: Event ()
    , _ilAdded   :: Event ()
    , _ilDeleted :: Event ()
    }

ingredientList :: TreeView -> TreeStore -> Garlic GarlicIngredientList
ingredientList view model = do
    -- DUMMY VALUES
    let insert :: MonadIO m => Double -> Text -> Text -> Bool -> m ()
        insert a b c d = do
            [b',c'] <- mapM (liftIO . toGValue . Just) [b,c]
            a' <- liftIO $ toGValue a
            d' <- liftIO $ toGValue d
            i  <- treeStoreAppend model Nothing
            treeStoreSet model i [0,1,2,3] [a',b',c',d']
     in replicateM_ 100 (insert 100 "g" "milk" False)
    -- /DUMMY VALUES
    
    units <- unitStore

    -- Build new Cell Renderers
    amntR <- new CellRendererText   [ #editable    := True ]
    unitR <- new CellRendererCombo  [ #editable    := True
                                    , #hasEntry    := False
                                    , #model       := units
                                    , #textColumn  := 0 ]
    nameR <- new CellRendererText   []
    optiR <- new CellRendererToggle [ #activatable := True ]

    -- Columns
    amntC <- new TreeViewColumn [ #title := "Amount" ]
    treeViewColumnPackStart amntC amntR False
    treeViewColumnAddAttribute amntC amntR "text" 0
    unitC <- new TreeViewColumn [ #title := "Unit" ]
    treeViewColumnPackStart unitC unitR False
    treeViewColumnAddAttribute unitC unitR "text" 1
    nameC <- new TreeViewColumn [ #title := "Name", #expand := True ]
    treeViewColumnPackStart nameC nameR True
    treeViewColumnAddAttribute nameC nameR "text" 2
    optiC <- new TreeViewColumn [ #title := "Optional" ]
    treeViewColumnPackStart optiC optiR False
    treeViewColumnAddAttribute optiC optiR "toggled" 3

    mapM_ (treeViewAppendColumn view) [ amntC, unitC, nameC, optiC ]

    return $ GarlicIngredientList never never never

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
