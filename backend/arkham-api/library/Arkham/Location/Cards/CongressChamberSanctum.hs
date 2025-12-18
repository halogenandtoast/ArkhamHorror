module Arkham.Location.Cards.CongressChamberSanctum (congressChamberSanctum) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Conspirator))

newtype CongressChamberSanctum = CongressChamberSanctum LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

congressChamberSanctum :: LocationCard CongressChamberSanctum
congressChamberSanctum = location CongressChamberSanctum Cards.congressChamberSanctum 0 (Static 0)

instance HasAbilities CongressChamberSanctum where
  getAbilities (CongressChamberSanctum a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> youExist can.search.deck)
      $ actionAbilityWithCost (ExhaustAssetCost $ AssetWithTrait Conspirator <> AssetControlledBy You)

instance RunMessage CongressChamberSanctum where
  runMessage msg l@(CongressChamberSanctum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] #asset (DrawFound iid 1)
      pure l
    _ -> CongressChamberSanctum <$> liftRunMessage msg attrs
