{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Garlic.View.IngredientEditor
(
    ingredientEditor,

    GarlicIngredientEditor,
    ieRun,
    ieDelete
)
where

import Control.Monad
import Control.Monad.Trans
import Reactive.Banana.GI.Gtk
import GI.Gtk
import Garlic.Types
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

uiIngredientEditor :: Text
uiIngredientEditor = decodeUtf8 $(embedFile "res/ingredient-editor.ui")

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

-- LENSES
makeGetters ''GarlicIngredientEditor
