{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.Presenter.RecipeDisplay
(
    recipeDisplayP
)
where

import Control.Lens
import Reactive.Banana
import Data.Functor.Contravariant
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed
import Database.Persist.Sql
import Text.Printf

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.RecipeDisplay
import Garlic.View.HeaderBar
import Garlic.Util
import Text.Blaze.Html

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Sequence as S

recipeStyle :: Text
recipeStyle = decodeUtf8 $(embedFile "res/style.css")

recipeDisplayP 
    :: GarlicApp 
    -> Behavior (Seq (Entity Recipe)) 
    -> Garlic ()
recipeDisplayP app rcps = do
    let disp = app ^. appRecipeDisplay

    -- Selection Event holding current recipe entity
    let selected = (S.index <$> rcps)
               <@> app ^. appRecipeList . recipeSelected

    -- Load instructions and reset spinner only on new selection
    disp ^. loadInstructions `consume` 
        fullInstructions . entityVal <$> selected
    app  ^. appHeader . changeYield `consume` 
        recipeYield . entityVal <$> selected

    -- Ingredients
    ingredients <- stepper [] =<< fetch ingredientsFor (entityKey <$> selected)
    weighed <- do
        ryield <- stepper 1 $ recipeYield . entityVal <$> selected
        syield <- stepper 1 $ app ^. appHeader . yieldChanged
        let factor = liftA2 (/) syield ryield
        pure $ scaleIngredients <$> factor <*> ingredients

    -- Display ingredient changes
    consume (replaceIngredients (app ^. appRecipeDisplay)) =<< 
        plainChanges weighed

replaceIngredients :: GarlicRecipeDisplay -> Consumer [WeighedIngredient]
replaceIngredients disp = mconcat
    [ disp ^. clearIngredients $< ()
    , map mkig >$< disp ^. addIngredients ]
    where mkig :: WeighedIngredient -> ViewIngredient
          mkig WeighedIngredient{..} = 
              let m = pack $ printf "%G %s" _wingrAmount _wingrUnit
               in ViewIngredient m (ingredientName _wingrIngr)

scaleIngredients :: Double -> [WeighedIngredient] -> [WeighedIngredient]
scaleIngredients factor = over (traverse . wingrAmount) (* factor)

fullInstructions :: Recipe -> Html
fullInstructions r = do
    H.style (text $ recipeStyle)
    H.div ! A.class_ "main" $ do
        H.h1 (text $ recipeName r)
        recipeHead r
        H.h2 "Instructions"
        toHtml (recipeInstructions r)

recipeHead :: Recipe -> Html
recipeHead Recipe{..} = H.dl $ do
    H.dt "Cuisine"
    H.dd (text recipeCuisine)

    H.dt "Rating"
    H.dd (string . ratingString $ recipeRating) 

    H.dt "Duration"
    H.dd (string . durationString $ recipeDuration)

    case recipeSource of
        Nothing -> return ()
        Just s  -> H.dt "Source" >> H.dd (text s)

    case recipeUrl of
        Nothing -> return ()
        Just s  -> H.dt "Website" >> H.dd (H.a ! A.href (textValue s) $ text s)
