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
    editCleanIngredient,
    editNewIngredient,
    editRegIngredient,
    editAddIngredient,
    editReplaceIngCompl,
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

    GarlicRecipeIngredient,
    irOptional,
    irAmount,
    irUnit,
    irDeleteClick,
)
where

import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk hiding (Unit)
import GI.GtkSource
import Garlic.Types
import Garlic.Data.Units
import Reactive.Banana.GI.Gtk
import Reactive.Banana (stepper)
import Reactive.Banana.Frameworks (mapEventIO, MomentIO, reactimate)
import Text.Markdown (Markdown (..))

uiRecipeEdit :: Text
uiRecipeEdit = decodeUtf8 $(embedFile "res/recipe-edit.ui")

uiIngredientNew :: Text
uiIngredientNew = decodeUtf8 $(embedFile "res/ingredient-new.ui")

uiIngredientEntryEdit :: Text
uiIngredientEntryEdit = decodeUtf8 $(embedFile "res/ingredient-entry-edit.ui")

data GarlicRecipeEdit = GarlicRecipeEdit
    { _showEditor          :: Consumer ()
    , _editSetInstructions :: Consumer Markdown
    , _editInstructions    :: Behavior (Maybe Markdown)
    , _editMasks           :: GarlicRecipeEditMask
    , _editCleanIngredient :: Consumer ()
    , _editNewIngredient   :: GarlicNewIngredient
    , _editRegIngredient   :: Fetcher (Double, Unit, Text) 
                                  GarlicRecipeIngredient
    , _editAddIngredient   :: Consumer GarlicRecipeIngredient
    , _editReplaceIngCompl :: Consumer [Text]
    , _editEnterIngredient :: Event Text
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
    ingredientList    <- castB b "ingredientList" ListBox
    ingredientSearch  <- castB b "ingredientSearch" Entry
    replaceCompletion <- ingredientCompletion ingredientSearch

    lift $ GarlicRecipeEdit
       <$> pure (ioConsumer (\_ -> stackSetVisibleChild stack redt))
       <*> pure (ioConsumer (\(Markdown t) -> set sbuf [ #text := toStrict t ]))
       <*> (fmap (fmap (Markdown . fromStrict)) <$> attrB sbuf #text)
       <*> pure masks
       <*> pure (ioConsumer $ \_ -> clearList ingredientList)
       <*> pure popover
       <*> pure (dynamicFetcher $ \(amount,name,unit) -> 
                    ingredientEntry ingredientList amount name unit)
       <*> pure (ioConsumer (\r -> listBoxInsert ingredientList (irRow r) (-1)))
       <*> pure replaceCompletion
       <*> ingredientEnter ingredientSearch
       <*> signalE0 deleteButton #clicked
       <*> signalE0 abortButton #clicked
       <*> signalE0 storeButton #clicked

    where clearList l = 
              containerGetChildren l >>= mapM_ (containerRemove l)

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

    where appendOne :: MonadIO m => ListStore -> Text -> m ()
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

data GarlicRecipeIngredient = GarlicRecipeIngredient
    { _irOptional    :: Behavior Bool
    , _irAmount      :: Behavior Double
    , _irUnit        :: Behavior Unit
    , _irDeleteClick :: Event ()
    , irRow          :: ListBoxRow
    }

ingredientEntry 
    :: ListBox
    -> Double 
    -> Unit
    -> Text 
    -> MomentIO GarlicRecipeIngredient
ingredientEntry lbox amount unit name = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientEntryEdit (-1)

    ingredientAmount   <- castB b "ingredientAmount" Entry
    ingredientUnit     <- castB b "ingredientUnit" ComboBoxText
    ingredientName     <- castB b "ingredientName" Label
    ingredientOptional <- castB b "ingredientOptional" CheckButton
    ingredientDelete   <- castB b "ingredientDelete" Button

    entrySetText ingredientAmount $ pack (show amount)
    labelSetLabel ingredientName name
    mapM_ (comboBoxTextAppendText ingredientUnit . prettyUnit) allUnits
    comboBoxSetActive ingredientUnit (fromIntegral $ fromEnum unit)

    row <- castB b "ingredientEntry" ListBoxRow
    _   <- on ingredientDelete #clicked $ containerRemove lbox row

    GarlicRecipeIngredient
       <$> attrB ingredientOptional #active
       <*> (fmap parseNum <$> attrB ingredientAmount #text)
       <*> comboBoxUnitB ingredientUnit
       <*> signalE0 ingredientDelete #clicked
       <*> pure row

-- LENSES
makeGetters ''GarlicRecipeEdit
makeGetters ''GarlicRecipeEditMask
makeGetters ''GarlicNewIngredient
makeGetters ''GarlicRecipeIngredient
