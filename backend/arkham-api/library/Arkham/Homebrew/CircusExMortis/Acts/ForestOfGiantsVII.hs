module Arkham.Homebrew.CircusExMortis.Acts.ForestOfGiantsVII (forestOfGiantsVII) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.ForestOfGiantsVI (
  flipPathForwardBesideRowOf,
  forestOfGiantsAbilities,
  forestOfGiantsModifiers,
  theCultEnMasseArrives,
 )
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies

-- | Same front as v.I; see 'Arkham.Homebrew.CircusExMortis.Acts.ForestOfGiantsVI'.
newtype ForestOfGiantsVII = ForestOfGiantsVII ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestOfGiantsVII :: ActCard ForestOfGiantsVII
forestOfGiantsVII = act (1, A) ForestOfGiantsVII Cards.forestOfGiantsVII Nothing

instance HasAbilities ForestOfGiantsVII where
  getAbilities = actAbilities forestOfGiantsAbilities

instance HasModifiersFor ForestOfGiantsVII where
  getModifiersFor (ForestOfGiantsVII a) = forestOfGiantsModifiers a

instance RunMessage ForestOfGiantsVII where
  runMessage msg a@(ForestOfGiantsVII attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipPathForwardBesideRowOf attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theCultEnMasseArrives attrs Enemies.theCultEnMasseBlackGoatsRapture
      pure a
    _ -> ForestOfGiantsVII <$> liftRunMessage msg attrs
