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
import Control.Monad.IO.Class
import Data.Monoid
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Reactive.Banana.GI.Gtk
import GI.Gtk
import GI.WebKit2

uiRecipeDisplay :: Text
uiRecipeDisplay = decodeUtf8 $(embedFile "res/recipe-display.ui")

uiIngredientEntry :: Text
uiIngredientEntry = decodeUtf8 $(embedFile "res/ingredient-entry.ui")

data GarlicRecipeDisplay = GarlicRecipeDisplay

recipeDisplay :: Stack -> Garlic GarlicRecipeDisplay
recipeDisplay stack = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeDisplay (-1)

    rdis <- castB b "recipeDisplayBox" Box
    vp <- castB b "instructionViewport" Viewport
    ingredients <- castB b "displayIngredients" FlowBox

    webview <- new WebView []
    setContainerChild vp webview

    --- DUMMY VALUES
    let attach x = flowBoxInsert ingredients x (-1)
     in mapM_ (attach =<<)
            [ groupSeparator "Sauce"
            , ingredientEntry "2 cups" "Chicken Stock"
            , ingredientEntry "3 tbsp" "Light Soy Sauce"
            , ingredientEntry "3 tbsp" "Dark Soy Sauce"
            , ingredientEntry "1 tbsp" "White Sugar"
            , ingredientEntry "½ tbsp" "Fish Sauce"
            , groupSeparator "Stir Fry"
            , ingredientEntry "350 g" "Beef"
            , ingredientEntry "1 large" "Bell Pepper"
            ]
    --- /DUMMY VALUES
    
    stackAddNamed stack rdis "recipeDisplay"

    pure GarlicRecipeDisplay

ingredientEntry :: MonadIO m => Text -> Text -> m ListBoxRow
ingredientEntry amount name = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientEntry (-1)

    amountL <- castB b "ingredientAmount" Label
    nameL   <- castB b "ingredientName" Label

    set amountL [ #label := amount ]
    set nameL [ #label := name ]

    castB b "ingredientEntry" ListBoxRow

groupSeparator :: MonadIO m => Text -> m ListBoxRow
groupSeparator name = do
    b <- builderNew
    _ <- builderAddFromString b uiIngredientEntry (-1)

    amountL <- castB b "ingredientAmount" Label
    nameL   <- castB b "ingredientName" Label

    set amountL [ #label := "" ]
    set nameL [ #label := "<b>" <> name <> "</b>"
              , #useMarkup := True ]

    castB b "ingredientEntry" ListBoxRow
