{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.RecipeDisplay
(
    GarlicRecipeDisplay,
    showDisplay,
    loadInstructions,
    recipeDisplay,

    ViewIngredient (..)
)
where

import Control.Monad.IO.Class
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import GI.Gtk
import GI.WebKit2
import Garlic.Types
import Reactive.Banana.GI.Gtk
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

uiRecipeDisplay :: Text
uiRecipeDisplay = decodeUtf8 $(embedFile "res/recipe-display.ui")

data GarlicRecipeDisplay = GarlicRecipeDisplay
    { _showDisplay      :: Consumer ()
    , _loadInstructions :: Consumer Html
    }

recipeDisplay :: Stack -> Garlic GarlicRecipeDisplay
recipeDisplay stack = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeDisplay (-1)

    -- Widgets
    rdis        <- castB b "recipeDisplayBox" Box
    vp          <- castB b "instructionViewport" Viewport

    -- WebView gets created in code
    webview <- new WebView []
    setContainerChild vp webview

    -- Add to provided stack
    stackAddNamed stack rdis "recipeDisplay"

    pure $ GarlicRecipeDisplay
           (ioConsumer (\_ -> stackSetVisibleChild stack rdis))
           (ioConsumer (setInstructions webview))

data ViewIngredient
    = ViewIngredient Text Text
    | ViewIngredientSep Text

setInstructions :: MonadIO m => WebView -> Html -> m ()
setInstructions view html = do
    let x = toStrict . renderHtml $ html
    webViewLoadHtml view x Nothing

-- LENSES
makeGetters ''GarlicRecipeDisplay
