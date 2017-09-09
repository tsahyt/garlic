{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.Presenter.RecipeDisplay
(
    recipeDisplayP
)
where

import Control.Lens
import Control.Monad
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql
import Reactive.Banana
import Text.Printf

import Garlic.Data.Duration
import Garlic.Data.Nutrition
import Garlic.Data.Units
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay
import Text.Blaze.Html

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

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

    -- Load instructions and reset spinner on new selection
    app  ^. appHeader . changeYield `consume` 
        recipeYield . entityVal <$> selected

    -- Event denoting a change in the weighed ingredients
    weighed <- do
        ryield <- stepper 1 $ recipeYield . entityVal <$> selected
        syield <- stepper 1 $ app ^. appHeader . yieldChanged
        let factor = liftA2 (/) syield ryield
        plainChanges (scaleIngredients <$> factor <*> ingredients)

    -- Behavior keeping track of the displayed data
    display <- accumB (error "no recipe") $ unions
        [ const <$> selected'
        , set _2 <$> weighed ]
    
    -- Display on changes
    consume (disp ^. loadInstructions) .
        fmap (uncurry fullInstructions . (over _1 entityVal)) =<< 
        plainChanges display

    -- DEBUG
    consume stdout . fmap (renderHtml . uncurry fullInstructions . (over _1 entityVal)) =<< 
        plainChanges display
    -- /DEBUG


scaleIngredients :: Double -> [WeighedIngredient] -> [WeighedIngredient]
scaleIngredients factor = over (traverse . wingrAmount) (* factor)

-- | Render a recipe to HTML.
fullInstructions :: Recipe -> [WeighedIngredient] -> Html
fullInstructions r is = do
    H.style (text $ recipeStyle)
    H.div ! A.class_ "main" $ do
        H.h1 (text $ recipeName r)
        recipeHead r
        H.details $ do
            H.summary "Nutrition"
            nutritionFacts (getNutrition defaultReferencePerson r is)
        H.h2 "Ingredients"
        ingredientList is
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

ingredientList :: [WeighedIngredient] -> Html
ingredientList is = H.ul ! A.id "ingredients" $
    forM_ is $ \i -> do
        let a = i ^. wingrAmount
            u = i ^. wingrUnit
            n = i ^. wingrIngr . to entityVal . to ingredientName
        H.li . string $ printf "%.2f%s %s" a (prettyUnit u :: String) n

data NEntry = Indent | IndentNoDV | Standard
    deriving (Show, Eq)

-- | Rendering of nutrition facts label
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
