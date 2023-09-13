module Arkham.Location.Cards.CourtOfTheGreatOldOnes (
  courtOfTheGreatOldOnes,
  CourtOfTheGreatOldOnes (..),
)
where

import Arkham.Prelude

import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Void))

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  locationWith
    CourtOfTheGreatOldOnes
    Cards.courtOfTheGreatOldOnes
    4
    (Static 6)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (CluesOnThis $ LessThan $ Static 6)
            $ ForcedAbility
            $ RoundEnds Timing.When
        , withTooltip
            "Shuffle this location into the Cosmos, moving each investigator and enemy that was at this location to Court of the Great Old Ones"
            $ restrictedAbility
              ( ProxySource
                  (LocationMatcherSource $ LocationWithTrait Void)
                  (toSource attrs)
              )
              1
              Here
              (ActionAbility Nothing $ ActionCost 1)
        ]

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg l@(CourtOfTheGreatOldOnes attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      let cluesToAdd = max 0 (6 - locationClues attrs)
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) cluesToAdd
      pure l
    UseCardAbility _ source@(ProxySource (LocationSource lid) (isSource attrs -> True)) 1 _ _ -> do
      investigators <- selectList $ investigatorAt lid
      card <- field Field.LocationCard lid
      pushAll
        $ [Move $ move (toAbilitySource source 1) iid (toId attrs) | iid <- investigators]
          <> [RemovedLocation lid, ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [card]]
      pure l
    _ -> CourtOfTheGreatOldOnes <$> runMessage msg attrs
