module Arkham.Location.Cards.SentinelPeak (
  sentinelPeak,
  SentinelPeak (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sentinelPeak)
import Arkham.Location.Runner
import Arkham.Matcher (LocationMatcher (..))
import Arkham.Trait

newtype SentinelPeak = SentinelPeak LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

sentinelPeak :: LocationCard SentinelPeak
sentinelPeak =
  locationWith SentinelPeak Cards.sentinelPeak 4 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 2) Anywhere]

instance RunMessage SentinelPeak where
  runMessage msg l@(SentinelPeak attrs) = case msg of
    InvestigatorDrewEncounterCard iid card -> do
      here <- iid `isAt` attrs
      player <- getPlayer iid
      pushWhen (here && Hex `member` toTraits card)
        $ chooseOne player [targetLabel attrs [assignDamage iid attrs 1]]
      pure l
    InvestigatorDrewPlayerCard iid card -> do
      here <- iid `isAt` attrs
      player <- getPlayer iid
      pushWhen (here && Hex `member` toTraits card)
        $ chooseOne player [targetLabel attrs [assignDamage iid attrs 1]]
      pure l
    _ -> SentinelPeak <$> runMessage msg attrs
