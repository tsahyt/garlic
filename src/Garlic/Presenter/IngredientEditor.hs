module Garlic.Presenter.IngredientEditor
(
    ingredientEditorP,
    currentIngredient
)
where

import Control.Lens
import Reactive.Banana

import Garlic.View
import Garlic.View.IngredientEditor
import Garlic.Types
import Garlic.Model
import Garlic.Model.Queries

ingredientEditorP :: GarlicApp -> Garlic ()
ingredientEditorP app = do
    let editor = app ^. appIngredientEd
        ingredient = currentIngredient $ editor ^. ieMask

    -- Filling the ComboBox
    inames <- fetch allIngredientNames $
        app ^. appStartup

    consume stdout . fmap show =<< plainChanges ingredient

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
