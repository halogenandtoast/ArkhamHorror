module Arkham.Location.Cards.SouthChurch_299 (
  southChurch_299,
  SouthChurch_299 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype SouthChurch_299 = SouthChurch_299 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_299 :: LocationCard SouthChurch_299
southChurch_299 = location SouthChurch_299 Cards.southChurch_299 2 (Static 0)

instance HasAbilities SouthChurch_299 where
  getAbilities (SouthChurch_299 attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (withBreaches attrs Here)
            $ ActionAbility Nothing
            $ ActionCost 1 <> DiscardAssetCost AnyAsset
        , withTooltip "You hide through the night." $ locationResignAction attrs
        ]

instance RunMessage SouthChurch_299 where
  runMessage msg l@(SouthChurch_299 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let breachCount = countLocationBreaches attrs
      act <- selectJust AnyAct
      pushAll
        [ InvestigatorDrawEncounterCard iid
        , RemoveBreaches (toTarget attrs) breachCount
        , PlaceBreaches (toTarget act) breachCount
        ]
      pure l
    _ -> SouthChurch_299 <$> runMessage msg attrs
