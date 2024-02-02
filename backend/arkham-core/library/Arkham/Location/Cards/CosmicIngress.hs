module Arkham.Location.Cards.CosmicIngress (
  cosmicIngress,
  CosmicIngress (..),
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

newtype CosmicIngress = CosmicIngress LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

cosmicIngress :: LocationCard CosmicIngress
cosmicIngress =
  locationWith
    CosmicIngress
    Cards.cosmicIngress
    2
    (Static 3)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CosmicIngress where
  getAbilities (CosmicIngress attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (CluesOnThis $ LessThan $ Static 3)
            $ ForcedAbility
            $ RoundEnds Timing.When
        , withTooltip
            "Shuffle this location into the Cosmos, moving each investigator and enemy that was at this location to Cosmic Ingress"
            $ restrictedAbility
              ( ProxySource
                  (LocationMatcherSource $ LocationWithTrait Void)
                  (toSource attrs)
              )
              1
              Here
              (ActionAbility [] $ ActionCost 1)
        ]

instance RunMessage CosmicIngress where
  runMessage msg l@(CosmicIngress attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      let cluesToAdd = max 0 (3 - locationClues attrs)
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) cluesToAdd
      pure l
    UseCardAbility _ source@(ProxySource (LocationSource lid) (isSource attrs -> True)) 1 _ _ -> do
      investigators <- selectList $ investigatorAt lid
      card <- field Field.LocationCard lid
      pushAll
        $ [Move $ move (toAbilitySource source 1) iid (toId attrs) | iid <- investigators]
        <> [RemovedLocation lid, ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [card]]
      pure l
    _ -> CosmicIngress <$> runMessage msg attrs
