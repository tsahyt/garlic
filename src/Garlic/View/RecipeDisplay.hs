{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.RecipeDisplay
(
    GarlicRecipeDisplay,
    showDisplay,
    loadInstructions,
    addIngredients,
    clearIngredients,
    recipeDisplay,

    ViewIngredient (..)
)
where

import Control.Monad.IO.Class
import Data.FileEmbed
import Data.Monoid
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

uiIngredientEntry :: Text
uiIngredientEntry = decodeUtf8 $(embedFile "res/ingredient-entry.ui")

data GarlicRecipeDisplay = GarlicRecipeDisplay
    { _showDisplay      :: Consumer ()
    , _loadInstructions :: Consumer Html
    , _addIngredients   :: Consumer [ViewIngredient]
    , _clearIngredients :: Consumer ()
    }

recipeDisplay :: Stack -> Garlic GarlicRecipeDisplay
recipeDisplay stack = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeDisplay (-1)

    -- Widgets
    rdis        <- castB b "recipeDisplayBox" Box
    vp          <- castB b "instructionViewport" Viewport
    ingredients <- castB b "displayIngredients" FlowBox

    -- WebView gets created in code
    webview <- new WebView []
    setContainerChild vp webview

    -- Add to provided stack
    stackAddNamed stack rdis "recipeDisplay"

    pure $ GarlicRecipeDisplay
           (ioConsumer (\_ -> stackSetVisibleChild stack rdis))
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

setInstructions :: MonadIO m => WebView -> Html -> m ()
setInstructions view html = do
    let x = toStrict . renderHtml $ html
    webViewLoadHtml view x Nothing

-- LENSES
makeGetters ''GarlicRecipeDisplay
