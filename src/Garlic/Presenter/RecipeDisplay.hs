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

-- TODO: Load dynamically from config location
recipeStyle :: Text
recipeStyle = decodeUtf8 $(embedFile "res/style.css")

recipeDisplayP 
    :: GarlicApp 
    -> Event (Entity Recipe)
    -> Garlic ()
recipeDisplayP app selected = do
    let disp = app ^. appRecipeDisplay

    -- Ingredients
    selected' <- fetch (fetchThrough (lmap entityKey ingredientsFor)) selected
    ingredients <- stepper [] (snd <$> selected')

    -- Load instructions and reset spinner only on new selection
    app  ^. appHeader . changeYield `consume` 
        recipeYield . entityVal <$> selected
    disp ^. loadInstructions `consume` 
        uncurry fullInstructions . (over _1 entityVal) <$> selected'

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
               in ViewIngredient m (ingredientName . entityVal $ _wingrIngr)

scaleIngredients :: Double -> [WeighedIngredient] -> [WeighedIngredient]
scaleIngredients factor = over (traverse . wingrAmount) (* factor)

-- | Render a recipe to HTML.
fullInstructions :: Recipe -> [WeighedIngredient] -> Html
fullInstructions r is = do
    H.style (text $ recipeStyle)
    H.div ! A.class_ "main" $ do
        H.h1 (text $ recipeName r)
        recipeHead r
        nutritionFacts (getNutrition defaultReferencePerson r is)
        H.h2 "Instructions"
        toHtml (recipeInstructions r)

-- | Rendering of Recipe Head
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

nutritionFacts :: NutritionLabel Double -> Html
nutritionFacts nl = H.div ! A.id "nutrition" $
    H.table $ H.tbody $ do
        H.tr $ H.td ! A.class_ "header" $ "Nutrition Facts"
        H.tr $ H.td $ 
            H.div ! A.class_ "serving" $ text ("Per " `mappend` nlServing nl)

        thickBar

        H.tr $ H.td $ do
            H.div ! A.class_ "line" $ 
                H.div ! A.class_ "label" $ "Amount Per Serving"
        H.tr $ H.td $ do
            H.div ! A.class_ "line" $ do
                H.div ! A.class_ "label" $ "Calories"
                string (printf "%.0f" (nlKcal nl))
                H.div ! A.class_ "labellight" $ 
                    string (printf "Calories from Fat %.0f" (nlKcalFat nl))
        H.tr $ H.td "% Daily Value"

        labelled False "Total Fat" (nlFat nl)
        labelled True "Saturated Fat" (nlSatFat nl)
        labelled True "Trans Fat" (nlTransFat nl)
        labelled False "Total Carbohydrates" (nlCarbs nl)
        labelled True "Dietary Fibre" (nlFibre nl)
        labelled True "Sugars" (nlSugars nl)
        labelled False "Protein" (nlProtein nl)

        thickBar

    where thickBar = H.tr ! A.style "height: 7px" $ 
              H.td ! A.style "background: #000;" $ pure ()
          
          labelled ind label (NVec v dv) =
              H.tr $ H.td ! (if ind then A.class_ "indent" else mempty) 
                $ H.div ! A.class_ "line" $ do
                  H.div ! A.class_ "label" $ do
                      text label 
                      H.div ! A.class_ "weight" $ string (printf "%.1fg" v)
                  H.div ! A.class_ "dv" $ string (printf "%.0f%%" (100 * dv))
