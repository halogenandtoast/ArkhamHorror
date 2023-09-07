module Arkham.Location.Cards.SouthChurch_298 (
  southChurch_298,
  SouthChurch_298 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Data.Monoid.Extra (mwhen)

newtype SouthChurch_298 = SouthChurch_298 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_298 :: LocationCard SouthChurch_298
southChurch_298 = location SouthChurch_298 Cards.southChurch_298 1 (Static 0)

instance HasAbilities SouthChurch_298 where
  getAbilities (SouthChurch_298 attrs) =
    let breachCount = countLocationBreaches attrs
     in withRevealedAbilities attrs
          $ [ restrictedAbility attrs 1 (Here <> mwhen (breachCount > 0) EncounterDeckIsNotEmpty)
                $ ActionAbility Nothing
                $ ActionCost 1
            , withTooltip "You hide through the night." $ locationResignAction attrs
            ]

instance RunMessage SouthChurch_298 where
  runMessage msg l@(SouthChurch_298 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let breachCount = countLocationBreaches attrs
      act <- selectJust AnyAct
      pushAll
        [ InvestigatorDrawEncounterCard iid
        , RemoveBreaches (toTarget attrs) breachCount
        , PlaceBreaches (toTarget act) breachCount
        ]
      pure l
    _ -> SouthChurch_298 <$> runMessage msg attrs
