{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.RecipeDisplay
(
    GarlicRecipeDisplay,
    loadInstructions,
    addIngredients,
    clearIngredients,
    recipeDisplay,

    ViewIngredient (..)
)
where

import Garlic.Types
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Monoid
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Reactive.Banana.GI.Gtk
import Text.Markdown (Markdown)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import GI.Gtk
import GI.WebKit2

uiRecipeDisplay :: Text
uiRecipeDisplay = decodeUtf8 $(embedFile "res/recipe-display.ui")

uiIngredientEntry :: Text
uiIngredientEntry = decodeUtf8 $(embedFile "res/ingredient-entry.ui")

data GarlicRecipeDisplay = GarlicRecipeDisplay
    { _loadInstructions :: Consumer Markdown
    , _addIngredients   :: Consumer [ViewIngredient]
    , _clearIngredients :: Consumer ()
    }

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
    setInstructions webview
        "1. thing\n2. another thing"
    --- /DUMMY VALUES
    
    stackAddNamed stack rdis "recipeDisplay"

    pure $ GarlicRecipeDisplay
           (ioConsumer (setInstructions webview))
           (ioConsumer (mapM_ (addViewIngredient ingredients)))
           (ioConsumer $ \_ -> do
                xs <- containerGetChildren ingredients 
                mapM_ (containerRemove ingredients) xs)

data ViewIngredient
    = ViewIngredient Text Text
    | ViewIngredientSep Text

addViewIngredient :: MonadIO m => FlowBox -> ViewIngredient -> m ()
addViewIngredient box ingr = do
    row <- case ingr of
               ViewIngredient amount name -> ingredientEntry amount name
               ViewIngredientSep title -> groupSeparator title
    flowBoxInsert box row (-1)

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

setInstructions :: MonadIO m => WebView -> Markdown -> m ()
setInstructions view md = do
    let x = toStrict . renderHtml . toHtml $ md
    webViewLoadHtml view x Nothing

makeGetters ''GarlicRecipeDisplay
