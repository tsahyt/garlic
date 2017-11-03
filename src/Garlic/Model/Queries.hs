{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Garlic.Model.Queries
(
    WeighedIngredient (..),
    fromIngredient,
    wingrAmount,
    wingrUnit,
    wingrIngr,
    wingrOptional,
    wingrDisp,
    wingrGroup,

    -- * Recipes
    recipes,
    ingredientsFor,
    newRecipe,
    updateRecipe,
    deleteRecipe,
    getRecipe,

    -- * Recipes/Ingredients for FoodLog
    recipeShort,

    -- * Ingredients
    allIngredientNames,
    ingredientByName,
    ingredientsByName,
    newIngredient,
    deleteIngredient,
    updateIngredient,
    getIngredient,

    -- * Weight Measurements
    getWeightMeasurements,
    addWeightMeasurement,
    deleteWeightMeasurement,

    -- * Goals
    getGoals,
    addGoal,
    deleteGoal,

    -- * FoodLog
    addFoodEntry,
    getFoodEntries,
    deleteFoodEntry,
    getEntryDays,
    changeAmountFoodEntry,

    -- * Nutrition Summary
    getNutritionSummary,
    getPastNutrition
)
where

import Control.Lens (over)
import Control.Lens.TH
import Control.Monad
import Garlic.Data.Units
import Garlic.Model
import Garlic.Data.Meal
import Garlic.Types
import Data.List (sortBy)
import Data.Foldable
import Data.Time
import Data.Ord
import Data.Maybe (catMaybes, fromJust)
import Database.Esqueleto
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Map (Map)
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Database.Persist as P

-- | Weighted ingredients are ingredients with an amount and a unit.
data WeighedIngredient = WeighedIngredient
    { _wingrAmount   :: Double
    , _wingrUnit     :: Unit
    , _wingrOptional :: Bool
    , _wingrDisp     :: Maybe Text
    , _wingrGroup    :: Maybe Text
    , _wingrIngr     :: Entity Ingredient
    }
    deriving Show

fromIngredient :: Entity Ingredient -> WeighedIngredient
fromIngredient e@(Entity _ v) = 
    WeighedIngredient 
        (ingredientBasicAmount v)
        (ingredientBasicUnit v)
        False
        Nothing
        Nothing
        e

makeLenses ''WeighedIngredient

-- | Fetcher to obtain all recipes with their associated weighted ingredients
-- from the database. The query is done in two steps, once for all recipes, then
-- as a loop over those recipes, rather than joining and then collapsing by
-- recipe.
recipes :: Text -> SqlPersistT IO (Seq (Entity Recipe))
recipes str = do
    let str' = flip T.snoc '%' . T.cons '%' $ str
    rs <- select $ 
          from $ \r -> do
              where_ (r ^. RecipeName `like` val str')
              return r
    pure . S.fromList $ rs

getRecipe :: Key Recipe -> SqlPersistT IO (Entity Recipe)
getRecipe = getJustEntity

-- | Select all weighted ingredients for some recipe
ingredientsFor :: Key Recipe -> SqlPersistT IO [WeighedIngredient]
ingredientsFor recipe = do
    xs <- select $
            from $ \(h,i) -> do
                where_ (h ^. RecipeHasRecipe ==. val recipe
                    &&. i ^. IngredientId ==. h ^. RecipeHasIngredient)

                return (h, i)

    pure $
        map
            (\(Entity _ RecipeHas{..}, i) ->
                 WeighedIngredient
                     recipeHasAmount
                     recipeHasUnit
                     recipeHasOptional
                     recipeHasDisplay
                     recipeHasGroup
                     i)
            xs

recipeShort ::
       Text -> SqlPersistT IO (Maybe (Entity Recipe, [WeighedIngredient]))
recipeShort t = do
    x <- P.selectFirst [RecipeName P.==. t] []
    case x of
        Nothing -> pure Nothing
        Just x' -> do
            ws <- ingredientsFor (entityKey x')
            pure $ Just (x', ws)

-- | Select all ingredient names in the DB
allIngredientNames :: SqlPersistT IO [Text]
allIngredientNames = map (ingredientName . entityVal) <$> P.selectList [] []

ingredientByName :: Text -> SqlPersistT IO (Maybe (Entity Ingredient))
ingredientByName name = P.selectFirst [IngredientName P.==. name] []

ingredientsByName :: [Text] -> SqlPersistT IO [Entity Ingredient]
ingredientsByName names = do
    xs <- mapM (\n -> P.selectFirst [IngredientName P.==. n] []) names
    pure $ catMaybes xs

updateRecipe :: Consumer (Entity Recipe, [WeighedIngredient])
updateRecipe =
    dbConsumer $ \(Entity k r, is)
    -- update the recipe entry
     -> do
        P.repsert k r
    -- remove all old associations to ingredients
        delete $ from $ \h -> where_ (h ^. RecipeHasRecipe ==. val k)
    -- insert new ones
        P.insertMany_
            [ RecipeHas
                k
                (entityKey _wingrIngr)
                _wingrAmount
                _wingrUnit
                _wingrOptional
                _wingrDisp
                (if maybe False T.null _wingrGroup
                     then Nothing
                     else _wingrGroup)
            | WeighedIngredient {..} <- is
            ]

newRecipe :: SqlPersistT IO (Entity Recipe)
newRecipe =
    P.insertEntity $ 
        Recipe "New Recipe" "Cuisine" 0 "" 0 1 "Serving" Nothing Nothing

deleteRecipe :: Consumer (Key Recipe)
deleteRecipe = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasRecipe ==. val k)
    delete $
        from $ \e ->
            where_ (e ^. FoodEntryRecipe ==. val (Just k))
    P.delete k

newIngredient :: Ingredient -> SqlPersistT IO (Maybe (Entity Ingredient))
newIngredient i = do
    x <- P.selectFirst [ IngredientName P.==. ingredientName i ] []
    case x of
        Just _  -> pure Nothing
        Nothing -> Just <$> P.insertEntity i

deleteIngredient :: Consumer (Key Ingredient)
deleteIngredient = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasIngredient ==. val k)
    P.delete k

updateIngredient :: Consumer (Key Ingredient, Ingredient)
updateIngredient = dbConsumer $ uncurry P.repsert

getIngredient :: Key Ingredient -> SqlPersistT IO (Entity Ingredient)
getIngredient = getJustEntity

getWeightMeasurements :: UTCTime -> SqlPersistT IO [Entity WeightMeasurement]
getWeightMeasurements l =
    sortBy (comparing (weightMeasurementTimestamp . entityVal)) <$>
    P.selectList [WeightMeasurementTimestamp P.>=. l] []

addWeightMeasurement :: WeightMeasurement -> SqlPersistT IO ()
addWeightMeasurement m = do
    x <-
        P.selectFirst
            [WeightMeasurementTimestamp P.==. weightMeasurementTimestamp m]
            []
    case x of
        Just e -> P.replace (entityKey e) m
        Nothing -> P.insert_ m

deleteWeightMeasurement :: UTCTime -> SqlPersistT IO ()
deleteWeightMeasurement t = P.deleteWhere [WeightMeasurementTimestamp P.==. t]

mapBy :: (Ord b, Foldable t) => (a -> b) -> t a -> Map b a
mapBy f = foldl' (\m x -> M.insert (f x) x m) M.empty

getGoals :: SqlPersistT IO (Map Day Goal)
getGoals = do
    xs <- P.selectList [] []
    pure $ mapBy (utctDay . goalTimestamp) (map entityVal xs)

addGoal :: Consumer Goal
addGoal =
    dbConsumer $ \g -> do
        x <-
            P.selectFirst
                [GoalTimestamp P.==. goalTimestamp g]
                []
        case x of
            Just e -> P.replace (entityKey e) g
            Nothing -> P.insert_ g

deleteGoal :: Consumer UTCTime
deleteGoal =
    dbConsumer $ \t -> P.deleteWhere [GoalTimestamp P.==. t]

addFoodEntry ::
       FoodEntry
    -> SqlPersistT IO (Entity FoodEntry)
addFoodEntry = P.insertEntity

getFoodEntries :: UTCTime -> SqlPersistT IO [Entity FoodEntry]
getFoodEntries t = P.selectList [FoodEntryTimestamp P.==. t] []

deleteFoodEntry :: Key FoodEntry -> SqlPersistT IO ()
deleteFoodEntry k = P.delete k >> pure ()

changeAmountFoodEntry :: Consumer (Double, Key FoodEntry)
changeAmountFoodEntry = dbConsumer $ \(a,k) ->
    P.update k [ FoodEntryAmount P.=. a ]

getEntryDays :: SqlPersistT IO [UTCTime]
getEntryDays = do
    xs <-
        select $
        from $ \e -> do
            groupBy (e ^. FoodEntryTimestamp)
            return (e ^. FoodEntryTimestamp)
    pure $ map unValue xs

getNutritionSummary :: UTCTime -> SqlPersistT IO [(Double, [WeighedIngredient])]
getNutritionSummary t
 = do
    -- recipe entries
    rs <-
        select $
        from $ \(e `InnerJoin` r) -> do
            on (e ^. FoodEntryRecipe ==. just (r ^. RecipeId))
            where_ (e ^. FoodEntryTimestamp ==. val t)
            pure (e, r)
    recipeData <-
        forM rs $ \(e, r) -> do
            let amount = foodEntryAmount . entityVal $ e
                yield = recipeYield . entityVal $ r
            is <- ingredientsFor (entityKey r)
            pure (amount / yield, is)
    -- ingredient entries
    is <-
        select $
        from $ \(e `InnerJoin` i) -> do
            on (e ^. FoodEntryIngredient ==. just (i ^. IngredientId))
            where_ (e ^. FoodEntryTimestamp ==. val t)
            pure (e, i)
    ingredientData <-
        forM is $ \(e, i) -> do
            let amount = foodEntryAmount . entityVal $ e
                unit = fromJust . foodEntryUnit . entityVal $ e
            pure (1, [WeighedIngredient amount unit False Nothing Nothing i])
    pure $ recipeData ++ ingredientData

getPastNutrition ::
       [UTCTime] -> SqlPersistT IO [(UTCTime, [(Double, [WeighedIngredient])])]
getPastNutrition = mapM (\t -> (,) <$> pure t <*> getNutritionSummary t)
