{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.Presenter.RecipeDisplay
(
    recipeDisplayP
)
where

import Control.Monad
import Control.Lens
import Reactive.Banana
import Data.Functor.Contravariant
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed
import Database.Persist.Sql
import Text.Printf

import Garlic.Data.Nutrition
import Garlic.Data.Units
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.RecipeDisplay
import Garlic.View.HeaderBar
import Garlic.Util
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String

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
    stdout `consume` 
        renderHtml . uncurry fullInstructions . (over _1 entityVal) <$> selected'

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
              let m = pack $ printf "%.2f %s" _wingrAmount _wingrUnit
               in ViewIngredient m (ingredientName . entityVal $ _wingrIngr)

scaleIngredients :: Double -> [WeighedIngredient] -> [WeighedIngredient]
scaleIngredients factor = over (traverse . wingrAmount) (* factor)

-- | Render a recipe to HTML.
fullInstructions :: Recipe -> [WeighedIngredient] -> Html
fullInstructions r is = do
    H.style (text $ recipeStyle)
    H.div ! A.class_ "main" $ do
        nutritionFacts (getNutrition defaultReferencePerson r is)
        H.h1 (text $ recipeName r)
        recipeHead r
        H.h2 "Instructions"
        H.div ! A.class_ "instructions" $
            toHtml (recipeInstructions r)

-- | Rendering of Recipe Head
recipeHead :: Recipe -> Html
recipeHead Recipe{..} = H.div ! A.class_ "recipe-head" $ H.dl $ do
    H.dt "Cuisine"
    H.dd (text recipeCuisine)

    H.dt "Rating"
    H.dd (string . ratingString $ recipeRating) 

    H.dt "Duration"
    H.dd (text . durationString $ recipeDuration)

    case recipeSource of
        Nothing -> return ()
        Just s  -> do
            H.dt "Source"
            H.dd $ case recipeUrl of
                       Nothing -> text s
                       Just u -> H.a ! A.href (textValue u) $ text s

data NEntry = Indent | IndentNoDV | Standard
    deriving (Show, Eq)

nutritionFacts :: NutritionLabel Double -> Html
nutritionFacts nl = H.div ! A.id "nutrition" $ H.table $ do
    -- Header
    H.tr $ H.td ! A.class_ "header" $ "Nutrition Facts"
    thickLine
    H.tr $ H.td $ do
        H.div ! A.class_ "top-lbls" $ do
            H.div ! A.class_ "amount-lbl" $ text $ 
                "Amount per " `mappend` nlServing nl
            H.div ! A.class_ "cal-lbl" $ "Calories"
        H.div ! A.class_ "cal" $ string $ printf "%0.f" (nlKcal nl)

    mediumLine
    H.tr $ H.td ! A.class_ "pct-dv" $ "% Daily Value*"

    -- Entries
    entry Standard Gram "Total Fat" (nlFat nl)
    entry Indent Gram "Saturated Fat" (nlSatFat nl)
    entry IndentNoDV Gram "Trans Fat" (nlTransFat nl)
    entry Standard Milligram "Cholesterol" (nlCholesterol nl)
    entry Standard Milligram "Sodium" (nlSodium nl)

    entry Standard Gram "Total Carbohydrates" (nlCarbs nl)
    entry Indent Gram "Dietary Fibre" (nlFibre nl)
    entry Indent Gram "Sugars" (nlSugars nl)

    entry Standard Gram "Protein" (nlProtein nl)

    thickLine

    -- Footer
    H.tr $ H.td ! A.class_ "dv-expl" $
        "* The % Daily Value (DV) tells you how much a nutrient in a\
        \ serving of food contributes to a daily diet. You nutrition\
        \ preferences are used to calculate this value"

    H.tr $ H.td ! A.class_ "cl-expl" $ do
        H.div ! A.class_ "line" $ pure ()
        "Calories per gram:"
        H.br
        "Fat 9 ⚫ Carbohydrate 4 ⚫ Protein 4"

    where thickLine = H.tr ! A.class_ "thick-line" $ 
                          H.td ! A.class_ "solid" $ pure ()
          mediumLine = H.tr ! A.class_ "medium-line" $ 
                          H.td ! A.class_ "solid" $ pure ()
          
          entry ty unit lbl (NVec g dv) = H.tr $ H.td $ do
              let cl = case ty of
                           Standard -> "label"
                           _        -> "llabel"
              H.div ! A.class_ "line" $ pure ()
              H.span ! A.class_ cl $ text lbl
              string $ printf "%.1f%s" g (prettyUnit unit :: String)
              unless (ty == IndentNoDV) $
                  H.span ! A.class_ "dv" $ string $
                      printf "%.0f%%" (dv * 100)
