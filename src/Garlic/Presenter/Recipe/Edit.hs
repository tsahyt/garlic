{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Garlic.Presenter.Recipe.Edit
(
    recipeEditP
)
where

import Control.Category
import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Functor.Compose
import Data.Sequence (Seq)
import Database.Persist.Sql
import Reactive.Banana

import Garlic.Data.Duration
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Presenter.IngredientEditor
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.IngredientEditor
import Garlic.View.Recipe
import Garlic.View.Recipe.Display
import Garlic.View.Recipe.Edit

import Prelude hiding ((.), id)

import qualified Data.Sequence as S

recipeEditP
    :: GarlicApp
    -> Event (Entity Recipe)
    -> Garlic (Event (Seq (Entity Recipe) -> Seq (Entity Recipe), 
               Entity Recipe))
recipeEditP app selected = do
    key <- stepper Nothing (Just . entityKey <$> selected)

    -- Show Editor on Edit or Add Click, hide Edit Button and yield
    let click = app ^. appHeader . editClick
            <:> app ^. appHeader . addClick
     in do app ^. appVRecipes . vrRecipeEdit . showEditor `consume` click
           app ^. appHeader . yieldToggle `consume` click
           app ^. appHeader . editToggle `consume` click
           app ^. appHeader . addToggle `consume` click
           app ^. appHeader . backToggle `consume` click

    recipe <- currentRecipe app
    
    -- Ingredients
    new <- newIngredientPopover app
    selectedIngredients <- loadRecipe app selected
    completion <- fetch allIngredientNames $ () <$ new
    app ^. appReplaceIngr `consume` completion

    entered <- fetch ingredientByName 
        $ app ^. appVRecipes . vrRecipeEdit . editEnterIngredient

    ingredients <- ingredientList 
        (app ^. appVRecipes . vrRecipeEdit . editIngredients) 
        selectedIngredients
        (new <:> entered)

    -- Recipe Entity, only Just when there is also a previous selection
    let recipeEntity = getCompose $ 
            (Entity <$> Compose key 
                    <*> Compose (Just <$> recipe))
    
    -- Save on Store Click, or do nothing when there was no selection
    storeE <- fetch (fetchThrough ingredients) 
            $ filterJust 
            $ recipeEntity <@ app ^. appVRecipes . vrRecipeEdit . editStore
    updateRecipe `consume` storeE

    -- Show Display on Abort Click
    revertDisplay app $ app ^. appHeader . backClick

    -- Delete Selected Recipe on Delete, revert to display view
    let deleteE = filterJust 
            (key <@ app ^. appVRecipes . vrRecipeEdit . editDelete)
    deleteRecipe `consume` deleteE
    revertDisplay app deleteE

    let change = unions
            [ (\x -> S.filter ((/= x) . entityKey)) <$> deleteE 
            , (\x -> fmap (\e -> if entityKey e == entityKey x then x else e)) 
            . fst <$> storeE ]

    let zipR = go <$> recipeEntity
            where go Nothing  _ = Nothing 
                  go (Just b) a = Just (a,b)
     in pure . filterJust $ zipR <@> change

revertDisplay :: GarlicApp -> Event a -> Garlic ()
revertDisplay app e = do
    app ^. appVRecipes . vrRecipeDisplay . showDisplay `consume` () <$ e
    app ^. appHeader . yieldToggle `consume` () <$ e
    app ^. appHeader . editToggle `consume` () <$ e
    app ^. appHeader . addToggle  `consume` () <$ e
    app ^. appHeader . backToggle  `consume` () <$ e

-- | Load recipe into mask on selection event
loadRecipe 
    :: GarlicApp 
    -> Event (Entity Recipe) 
    -> Garlic (Event [WeighedIngredient])
loadRecipe app selected = do
    let masks = app ^. appVRecipes . vrRecipeEdit . editMasks
        rcp   = entityVal <$> selected
    masks ^. editSetName `consume` recipeName <$> rcp
    masks ^. editSetCuisine `consume` recipeCuisine <$> rcp
    masks ^. editSetDuration `consume` durationString . recipeDuration <$> rcp
    masks ^. editSetYield `consume` recipeYield <$> rcp
    masks ^. editSetYieldUnit `consume` recipeYieldUnit <$> rcp
    masks ^. editSetSource `consume` fromMaybe "" . recipeSource <$> rcp
    masks ^. editSetURL `consume` fromMaybe "" . recipeUrl <$> rcp
    masks ^. editSetRating `consume` recipeRating <$> rcp

    app ^. appVRecipes . vrRecipeEdit . editSetInstructions 
        `consume` recipeInstructions <$> rcp

    fetch ingredientsFor (entityKey <$> selected)

-- | The currently edited recipe
currentRecipe :: GarlicApp -> Garlic (Behavior Recipe)
currentRecipe app = do
    let masks = app ^. appVRecipes . vrRecipeEdit . editMasks
        r     = Recipe 
            <$> masks ^. editName
            <*> masks ^. editCuisine
            <*> masks ^. editRating
            <*> fmap (fromMaybe "") 
                    (app ^. appVRecipes . vrRecipeEdit . editInstructions)
            <*> fmap (fromMaybe 0 . parseDuration) (masks ^. editDuration)
            <*> masks ^. editYield
            <*> masks ^. editYieldUnit
            <*> fmap mtext (masks ^. editSource)
            <*> fmap mtext (masks ^. editURL)
    pure r

-- | Network describing the new ingredient dialog
newIngredientPopover :: GarlicApp -> Garlic (Event (Entity Ingredient))
newIngredientPopover app = do
    let ni = app ^. appVRecipes . vrRecipeEdit . editNewIngredient

    ni ^. niMask ^. imClearAll `consume` ni ^. niClearClick
    ni ^. niMask ^. imClearAll `consume` ni ^. niOkClick

    new <- fetch newIngredient $ 
        currentIngredient (ni ^. niMask) <@ ni ^. niOkClick
    app ^. appDisplayError `consume`
        "Ingredient already exists!" <$ filterE isNothing new
    
    pure (filterJust new)

-- | Management of the ingredient list
ingredientList 
    :: GarlicIngredientList 
    -> Event [WeighedIngredient] 
    -> Event (Entity Ingredient)
    -> Garlic (Fetcher a [WeighedIngredient])
ingredientList ilist selected new = mdo
    let fromWI w = EditorIngredient
                       (view wingrAmount w)
                       (view wingrUnit w)
                       (view (wingrIngr . to entityVal . to ingredientName) w)
                       (view wingrDisp w)
                       (view wingrGroup w)
                       (view wingrOptional w)

        toWI e i = WeighedIngredient
                       (eiAmount e)
                       (eiUnit e)
                       (eiOptional e)
                       (eiDisplay e)
                       (eiGroup e)
                       i
        
    -- Replace ingredient list on new selection
    ((() >$ ilist ^. ilClear) <> ilist ^. ilAppend) `consume` 
        map fromWI <$> selected

    -- Insert new ingredients
    ilist ^. ilAppend `consume` (pure . fromWI . fromIngredient) <$> new

    pure $ rmap (uncurry (zipWith toWI)) 
         $ fetchThrough (lmap (map eiName) ingredientsByName)
         . lmap (const ()) (ilist ^. ilFetch)
