module Arkham.Location.Cards.HallOfIdolatry (hallOfIdolatry) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype HallOfIdolatry = HallOfIdolatry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfIdolatry :: LocationCard HallOfIdolatry
hallOfIdolatry = symbolLabel $ location HallOfIdolatry Cards.hallOfIdolatry 3 (PerPlayer 2)

instance HasAbilities HallOfIdolatry where
  getAbilities (HallOfIdolatry attrs) =
    extendRevealed1 attrs
      $ mkAbility attrs 1
      $ forced
      $ Explored #after You Anywhere
      $ SuccessfulExplore (be attrs)

instance RunMessage HallOfIdolatry where
  runMessage msg l@(HallOfIdolatry attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      yigsFury <- getRecordCount YigsFury
      when (yigsFury >= 5) $ drawEncounterCards iid attrs (if yigsFury >= 10 then 2 else 1)
      pure l
    _ -> HallOfIdolatry <$> liftRunMessage msg attrs
