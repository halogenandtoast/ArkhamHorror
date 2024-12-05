module Arkham.Location.Cards.ShrineToHydra (shrineToHydra, ShrineToHydra (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ShrineToHydra = ShrineToHydra LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineToHydra :: LocationCard ShrineToHydra
shrineToHydra = location ShrineToHydra Cards.shrineToHydra 5 (PerPlayer 2)

instance HasModifiersFor ShrineToHydra where
  getModifiersFor (ShrineToHydra a) = do
    self <- modifySelfMaybe a do
      liftGuardM $ selectAny $ at_ (be a) <> InvestigatorWithKey GreenKey
      pure [ShroudModifier (-3)]
    investigators <- modifySelect a Anyone [CannotDiscoverCluesExceptAsResultOfInvestigation (be a)]
    pure $ self <> investigators

instance HasAbilities ShrineToHydra where
  getAbilities (ShrineToHydra a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage ShrineToHydra where
  runMessage msg l@(ShrineToHydra attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs RedKey
      pure l
    _ -> ShrineToHydra <$> liftRunMessage msg attrs
