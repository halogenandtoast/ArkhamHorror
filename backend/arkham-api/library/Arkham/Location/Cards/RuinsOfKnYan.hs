module Arkham.Location.Cards.RuinsOfKnYan (ruinsOfKnYan) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RuinsOfKnYan = RuinsOfKnYan LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfKnYan :: LocationCard RuinsOfKnYan
ruinsOfKnYan = location RuinsOfKnYan Cards.ruinsOfKnYan 4 (PerPlayer 2)

instance HasAbilities RuinsOfKnYan where
  getAbilities (RuinsOfKnYan a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        (Here <> thisExists a LocationWithAnyClues <> youExist (InvestigatorWithSupply Compass))
        doubleActionAbility

instance RunMessage RuinsOfKnYan where
  runMessage msg l@(RuinsOfKnYan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAt NotInvestigate iid (attrs.ability 1) 2 attrs
      pure l
    _ -> RuinsOfKnYan <$> liftRunMessage msg attrs
