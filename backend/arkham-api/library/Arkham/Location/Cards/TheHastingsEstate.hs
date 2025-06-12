module Arkham.Location.Cards.TheHastingsEstate (theHastingsEstate) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheHastingsEstate = TheHastingsEstate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHastingsEstate :: LocationCard TheHastingsEstate
theHastingsEstate = location TheHastingsEstate Cards.theHastingsEstate 3 (PerPlayer 2)

instance HasAbilities TheHastingsEstate where
  getAbilities (TheHastingsEstate a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> youExist (InvestigatorWithSupply Medicine) <> thisExists a LocationWithAnyClues)
        (actionAbilityWithCost $ SupplyCost (be a) Medicine)

instance RunMessage TheHastingsEstate where
  runMessage msg l@(TheHastingsEstate attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) attrs attrs.clues
      pure l
    _ -> TheHastingsEstate <$> liftRunMessage msg attrs
