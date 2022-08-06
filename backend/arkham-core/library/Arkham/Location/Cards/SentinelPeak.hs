module Arkham.Location.Cards.SentinelPeak
  ( sentinelPeak
  , SentinelPeak(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Cost
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( sentinelPeak )
import Arkham.Location.Runner
import Arkham.Matcher ( LocationMatcher (..) )
import Arkham.Message
import Arkham.Trait

newtype SentinelPeak = SentinelPeak LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sentinelPeak :: LocationCard SentinelPeak
sentinelPeak = locationWith
  SentinelPeak
  Cards.sentinelPeak
  4
  (PerPlayer 2)
  (costToEnterUnrevealedL
  .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 2) Anywhere]
  )

instance RunMessage SentinelPeak where
  runMessage msg l@(SentinelPeak attrs) = case msg of
    InvestigatorDrewEncounterCard iid card | iid `on` attrs -> l <$ when
      (Hex `member` toTraits card)
      (push $ TargetLabel
        (toTarget attrs)
        [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
      )
    InvestigatorDrewPlayerCard iid card | iid `on` attrs -> l <$ when
      (Hex `member` toTraits card)
      (push $ TargetLabel
        (toTarget attrs)
        [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
      )
    _ -> SentinelPeak <$> runMessage msg attrs
