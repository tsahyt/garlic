{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic.Presenter.Recipe.Edit
(
    recipeEditP
)
where

import Control.Lens
import Control.Monad.IO.Class
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

import qualified Data.Sequence as S

recipeEditP ::
       GarlicApp
    -> Event (Entity Recipe)
    -> Garlic (Event (Seq (Entity Recipe) 
                -> Seq (Entity Recipe), Entity Recipe))
recipeEditP app selected = do
    key <- stepper Nothing (Just . entityKey <$> selected)

    -- Show Editor on Edit or Add Click, hide Edit Button and yield
    let click = app ^. appHeader . editClick <:> app ^. appHeader . addClick
     in app ^. appVRecipes . vrRecipeEdit . showEditor `consume` click
    recipe <- currentRecipe app

    -- Ingredients
    new <- newIngredientPopover app
    selectedIngredients <- loadRecipe app selected
    completion <- dbFetch $ allIngredientNames <$ new
    app ^. appReplaceIngr `consume` completion
    entered <-
        filterJust <$$> dbFetch $ ingredientByName <$> app ^. appVRecipes .
        vrRecipeEdit .
        editEnterIngredient

    ingredients <-
        ingredientList
            (app ^. appVRecipes . vrRecipeEdit . editIngredients)
            selectedIngredients
            (new <:> entered)

    -- Recipe Entity, only Just when there is also a previous selection
    let recipeEntity =
            getCompose (Entity <$> Compose key <*> Compose (Just <$> recipe))

    -- Save on Store Click, or do nothing when there was no selection
    storeE <-
        dbFetch $ (\x -> (,) <$> pure x <*> ingredients) <$>
        filterJust
            (recipeEntity <@ app ^. appVRecipes . vrRecipeEdit . editStore)

    updateRecipe `consume` storeE
    storeE1 <- delay storeE

    -- Go back on back click
    app ^. appVRecipes . vrRecipeDisplay . showDisplay `consume` () <$ app ^.
        appHeader .
        backClick
        
    -- Delete Selected Recipe on Delete, revert to display view
    let deleteE =
            filterJust (key <@ app ^. appVRecipes . vrRecipeEdit . editDelete)
    deleteRecipe `consume` deleteE
    app ^. appVRecipes . vrRecipeDisplay . showDisplay `consume` () <$ deleteE
    let change =
            unions
                [ (\x -> S.filter ((/= x) . entityKey)) <$> deleteE
                , (\x ->
                       fmap
                           (\e ->
                                if entityKey e == entityKey x
                                    then x
                                    else e)) .
                  fst <$>
                  storeE1
                ]

    -- Delay change event for output, to guarantee database changes. hack.
    change' <- delay =<< delay change

    let zipR = go <$> recipeEntity
          where
            go Nothing _ = Nothing
            go (Just b) a = Just (a, b)
     in pure . filterJust $ zipR <@> change'

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

    dbFetch $ ingredientsFor . entityKey <$> selected

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

    new <- dbFetch $ newIngredient <$>
        currentIngredient (ni ^. niMask) <@ ni ^. niOkClick
    app ^. appDisplayError `consume`
        "Ingredient already exists!" <$ filterE isNothing new
    
    pure (filterJust new)

-- | Management of the ingredient list
ingredientList 
    :: GarlicIngredientList 
    -> Event [WeighedIngredient] 
    -> Event (Entity Ingredient)
    -> Garlic (SqlPersistT IO [WeighedIngredient])
ingredientList ilist selected new = mdo
    let fromWI w = EditorIngredient
                       (view wingrAmount w)
                       (view wingrUnit w)
                       (view (wingrIngr . to entityVal . to ingredientName) w)
                       (view wingrDisp w)
                       (view wingrGroup w)
                       (view wingrOptional w)

        toWI e   = WeighedIngredient
                       (eiAmount e)
                       (eiUnit e)
                       (eiOptional e)
                       (eiDisplay e)
                       (eiGroup e)
        
    -- Replace ingredient list on new selection
    ((() >$ ilist ^. ilClear) <> ilist ^. ilAppend) `consume` 
        map fromWI <$> selected

    -- Insert new ingredients
    ilist ^. ilAppend `consume` (pure . fromWI . fromIngredient) <$> new

    pure $ do
        xs <- liftIO $ ilist ^. ilFetch
        is <- mapM (ingredientByName . eiName) xs
        pure (catMaybes (zipWith (liftA2 toWI) (map Just xs) is))
