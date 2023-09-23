module Arkham.Location.Cards.HallOfIdolatry (
  hallOfIdolatry,
  HallOfIdolatry (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

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
          $ ForcedAbility
          $ Explored Timing.After You
          $ SuccessfulExplore
          $ LocationWithId
          $ toId attrs
      ]

instance RunMessage HallOfIdolatry where
  runMessage msg l@(HallOfIdolatry attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      yigsFury <- getRecordCount YigsFury
      when (yigsFury >= 5) $ do
        pushAll
          $ InvestigatorDrawEncounterCard iid
          : [InvestigatorDrawEncounterCard iid | yigsFury >= 10]
      pure l
    _ -> HallOfIdolatry <$> runMessage msg attrs
