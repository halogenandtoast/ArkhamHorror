module Arkham.Homebrew.CircusExMortis.Acts.ForestOfGiantsVIII (forestOfGiantsVIII) where

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
newtype ForestOfGiantsVIII = ForestOfGiantsVIII ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestOfGiantsVIII :: ActCard ForestOfGiantsVIII
forestOfGiantsVIII = act (1, A) ForestOfGiantsVIII Cards.forestOfGiantsVIII Nothing

instance HasAbilities ForestOfGiantsVIII where
  getAbilities = actAbilities forestOfGiantsAbilities

instance HasModifiersFor ForestOfGiantsVIII where
  getModifiersFor (ForestOfGiantsVIII a) = forestOfGiantsModifiers a

instance RunMessage ForestOfGiantsVIII where
  runMessage msg a@(ForestOfGiantsVIII attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipPathForwardBesideRowOf attrs iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theCultEnMasseArrives attrs Enemies.theCultEnMasseRingmastersFervor
      pure a
    _ -> ForestOfGiantsVIII <$> liftRunMessage msg attrs
