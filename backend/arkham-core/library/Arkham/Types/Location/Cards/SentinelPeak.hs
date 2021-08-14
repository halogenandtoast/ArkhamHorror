module Arkham.Types.Location.Cards.SentinelPeak
  ( sentinelPeak
  , SentinelPeak(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (sentinelPeak)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Trait

newtype SentinelPeak = SentinelPeak LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sentinelPeak :: LocationCard SentinelPeak
sentinelPeak = locationWith
  SentinelPeak
  Cards.sentinelPeak
  4
  (PerPlayer 2)
  Diamond
  [Square]
  (costToEnterUnrevealedL
  .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 2) Nothing]
  )

instance HasModifiersFor env SentinelPeak

instance ActionRunner env => HasAbilities env SentinelPeak where
  getAbilities iid window (SentinelPeak attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env SentinelPeak where
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
