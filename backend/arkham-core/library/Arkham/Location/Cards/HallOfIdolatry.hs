module Arkham.Location.Cards.HallOfIdolatry (hallOfIdolatry, HallOfIdolatry (..)) where

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype HallOfIdolatry = HallOfIdolatry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hallOfIdolatry :: LocationCard HallOfIdolatry
hallOfIdolatry = location HallOfIdolatry Cards.hallOfIdolatry 3 (PerPlayer 2)

instance HasAbilities HallOfIdolatry where
  getAbilities (HallOfIdolatry attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ forced
          $ Explored #after You
          $ SuccessfulExplore (be attrs)
      ]

instance RunMessage HallOfIdolatry where
  runMessage msg l@(HallOfIdolatry attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      yigsFury <- getRecordCount YigsFury
      pushWhen (yigsFury >= 5)
        $ drawEncounterCards iid attrs (if yigsFury >= 10 then 2 else 1)
      pure l
    _ -> HallOfIdolatry <$> runMessage msg attrs
