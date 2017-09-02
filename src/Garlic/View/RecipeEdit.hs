{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.RecipeEdit
(
    GarlicRecipeEdit,
    showEditor,
    editInstructions,
    recipeEdit,
)
where

import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe
import GI.Gtk
import GI.GtkSource
import Garlic.Types
import Reactive.Banana.GI.Gtk
import Text.Markdown (Markdown (..))

uiRecipeEdit :: Text
uiRecipeEdit = decodeUtf8 $(embedFile "res/recipe-edit.ui")

data GarlicRecipeEdit = GarlicRecipeEdit
    { _showEditor       :: Consumer ()
    , _editInstructions :: Behavior (Maybe Markdown)
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

    lift $ GarlicRecipeEdit
       <$> pure (ioConsumer (\_ -> stackSetVisibleChild stack redt))
       <*> (fmap (fmap (Markdown . fromStrict)) <$> attrB sbuf #text)

buildSourceView :: MonadIO m => m (View, Buffer)
buildSourceView = do
    lm <- languageManagerGetDefault
    markdown <- fromMaybe (error "Markdown Definition not found!") 
            <$> languageManagerGetLanguage lm "markdown"
    sbuf <- new Buffer [ #language := markdown ]
    sourceview <- viewNewWithBuffer sbuf
    set sourceview [ #showLineNumbers := True
                   , #smartBackspace := True
                   , #monospace := True
                   ]

    pure (sourceview, sbuf)

-- LENSES
makeGetters ''GarlicRecipeEdit
