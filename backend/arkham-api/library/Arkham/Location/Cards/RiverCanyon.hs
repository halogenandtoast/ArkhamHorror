module Arkham.Location.Cards.RiverCanyon (riverCanyon) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype RiverCanyon = RiverCanyon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverCanyon :: LocationCard RiverCanyon
riverCanyon = symbolLabel $ location RiverCanyon Cards.riverCanyon 4 (PerPlayer 1)

instance HasAbilities RiverCanyon where
  getAbilities (RiverCanyon a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> exists (HealableInvestigator (a.ability 1) #damage You)) actionAbility

instance RunMessage RiverCanyon where
  runMessage msg l@(RiverCanyon attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasCanteen <- getHasSupply iid Canteen
      healDamage iid (attrs.ability 1) (if hasCanteen then 3 else 1)
      pure l
    _ -> RiverCanyon <$> liftRunMessage msg attrs
