module Arkham.Location.Cards.CosmicIngress (cosmicIngress) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Void))

newtype CosmicIngress = CosmicIngress LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicIngress :: LocationCard CosmicIngress
cosmicIngress =
  locationWith CosmicIngress Cards.cosmicIngress 2 (Static 3) (connectsToL .~ adjacentLocations)

instance HasAbilities CosmicIngress where
  getAbilities (CosmicIngress a) =
    extendRevealed
      a
      [ restricted a 1 (CluesOnThis $ lessThan 3) $ forced $ RoundEnds #when
      , withTooltip
          "Shuffle this location into the Cosmos, moving each investigator and enemy that was at this location to Cosmic Ingress"
          $ restricted
            (proxied (LocationMatcherSource $ LocationWithTrait Void) a)
            1
            Here
            actionAbility
      ]

instance RunMessage CosmicIngress where
  runMessage msg l@(CosmicIngress attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs $ max 0 (3 - attrs.clues)
      pure l
    UseThisAbility _ source@(ProxySource (LocationSource lid) (isSource attrs -> True)) 1 -> do
      selectEach (investigatorAt lid) \iid -> moveTo (toAbilitySource source 1) iid attrs
      removeLocation lid
      card <- field Field.LocationCard lid
      shuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [card]
      pure l
    _ -> CosmicIngress <$> liftRunMessage msg attrs
