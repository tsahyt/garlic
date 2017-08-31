{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.RecipeDisplay
(
    GarlicRecipeDisplay,
    recipeDisplay
)
where

import Garlic.Types
import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Reactive.Banana.GI.Gtk
import GI.Gtk
import GI.WebKit2

uiRecipeDisplay :: Text
uiRecipeDisplay = decodeUtf8 $(embedFile "res/recipe-display.ui")

data GarlicRecipeDisplay = GarlicRecipeDisplay

recipeDisplay :: Stack -> Garlic GarlicRecipeDisplay
recipeDisplay stack = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeDisplay (-1)

    rdis <- castB b "recipeDisplayBox" Box
    vp <- castB b "instructionViewport" Viewport

    webview <- new WebView []
    setContainerChild vp webview
    
    stackAddNamed stack rdis "recipeDisplay"

    pure GarlicRecipeDisplay
