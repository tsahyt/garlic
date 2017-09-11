module Garlic.Presenter.IngredientEditor
(
    ingredientEditorP,
    currentIngredient
)
where

import Control.Lens
import Garlic.Types
import Garlic.Model
import Reactive.Banana

import Garlic.View.IngredientEditor

ingredientEditorP
    :: GarlicIngredientEditor
    -> Garlic ()
ingredientEditorP editor = pure ()

-- | Behavior describing the current ingredient as specified in the new
-- ingredient popover.
currentIngredient :: GarlicIngredientMask -> Behavior Ingredient
currentIngredient mask = Ingredient
    <$> mask ^. imName
    <*> mask ^. imComment
    <*> mask ^. imUnit
    <*> (parseNum <$> mask ^. imAmount)
    <*> (parseNum <$> mask ^. imProtein)
    <*> (parseNum <$> mask ^. imCarbs)
    <*> (fmap parseNum . mtext <$> mask ^. imSugar)
    <*> (fmap parseNum . mtext <$> mask ^. imFibre)
    <*> (parseNum <$> mask ^. imFat)
    <*> (fmap parseNum . mtext <$> mask ^. imSatFat)
    <*> (fmap parseNum . mtext <$> mask ^. imPolyFat)
    <*> (fmap parseNum . mtext <$> mask ^. imMonoFat)
    <*> (fmap parseNum . mtext <$> mask ^. imTransFat)
    <*> (fmap parseNum . mtext <$> mask ^. imSodium)
    <*> (fmap parseNum . mtext <$> mask ^. imCholesterol)
