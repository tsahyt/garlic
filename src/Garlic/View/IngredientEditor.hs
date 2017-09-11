{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Garlic.View.IngredientEditor
(
    ingredientEditor,

    GarlicIngredientEditor,
    ieRun,
    ieDelete,

    ingredientMask,
    GarlicIngredientMask,
    imClearAll,
    imName,
    imComment,
    imAmount,
    imUnit,
    imProtein,
    imCarbs,
    imSugar,
    imFibre,
    imFat,
    imSatFat,
    imPolyFat,
    imMonoFat,
    imTransFat,
    imSodium,
    imCholesterol
)
where

import Control.Monad
import Control.Monad.Trans
import Reactive.Banana
import Reactive.Banana.Frameworks (mapEventIO, MomentIO)
import Reactive.Banana.GI.Gtk
import GI.Gtk hiding (Unit)
import Garlic.Types
import Garlic.Data.Units
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

uiIngredientEditor :: Text
uiIngredientEditor = decodeUtf8 $(embedFile "res/ingredient-editor.ui")

uiIngredientMask :: Text
uiIngredientMask = decodeUtf8 $(embedFile "res/ingredient-mask.ui")

data GarlicIngredientEditor = GarlicIngredientEditor
    { _ieRun    :: Consumer ()
    , _ieDelete :: Event ()
    }

ingredientEditor :: ApplicationWindow -> Garlic GarlicIngredientEditor
ingredientEditor win = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientEditor (-1)

    editor <- castB b "ingredientEditor" Dialog
    deleteButton <- castB b "deleteButton" Button

    windowSetTransientFor editor (Just win)

    GarlicIngredientEditor
        <$> pure (ioConsumer $ \_ -> void $ dialogRun editor)
        <*> lift (signalE0 deleteButton #clicked)

data GarlicIngredientMask = GarlicIngredientMask
    { _imClearAll    :: Consumer ()
    , _imName        :: Behavior Text
    , _imComment     :: Behavior Text
    , _imAmount      :: Behavior Text
    , _imUnit        :: Behavior Unit
    , _imProtein     :: Behavior Text
    , _imCarbs       :: Behavior Text
    , _imSugar       :: Behavior Text
    , _imFibre       :: Behavior Text
    , _imFat         :: Behavior Text
    , _imSatFat      :: Behavior Text
    , _imPolyFat     :: Behavior Text
    , _imMonoFat     :: Behavior Text
    , _imTransFat    :: Behavior Text
    , _imSodium      :: Behavior Text
    , _imCholesterol :: Behavior Text
    }

ingredientMask :: Box -> Garlic GarlicIngredientMask
ingredientMask box = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientMask (-1)

    mask     <- castB b "ingredientMask" Box
    name     <- castB b "name" Entry
    comment  <- castB b "comment" Entry
    unit     <- castB b "unit" ComboBoxText
    amount   <- castB b "amount" Entry
    protein  <- castB b "protein" Entry
    carbs    <- castB b "carbs" Entry
    sugar    <- castB b "sugar" Entry
    fibre    <- castB b "fibre" Entry
    fat      <- castB b "fat" Entry
    satFat   <- castB b "satFat" Entry
    polyFat  <- castB b "polyFat" Entry
    monoFat  <- castB b "monoFat" Entry
    transFat <- castB b "transFat" Entry
    sodium   <- castB b "sodium" Entry
    chlstrl  <- castB b "cholesterol" Entry

    mapM_ (comboBoxTextAppendText unit . prettyUnit) allUnits

    let clearAll = mapM_ (flip setEntryText "")
            [ name, comment, amount, protein, carbs, sugar, fibre
            , fat, satFat, polyFat, monoFat, transFat ]

    boxPackEnd box mask True True 0

    lift $ GarlicIngredientMask
       <$> pure (ioConsumer $ \_ -> clearAll)
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
       <*> attrB sodium #text
       <*> attrB chlstrl #text

comboBoxUnitB :: ComboBoxText -> MomentIO (Behavior Unit)
comboBoxUnitB box = do
    c  <- signalE0 box #changed
    c' <- mapEventIO (const $ comboBoxTextGetActiveText box) c
    stepper Gram $ parseUnit <$> c'

-- LENSES
makeGetters ''GarlicIngredientEditor
makeGetters ''GarlicIngredientMask
