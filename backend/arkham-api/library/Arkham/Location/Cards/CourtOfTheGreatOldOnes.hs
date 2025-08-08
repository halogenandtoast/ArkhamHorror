module Arkham.Location.Cards.CourtOfTheGreatOldOnes (courtOfTheGreatOldOnes) where

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

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  locationWith CourtOfTheGreatOldOnes Cards.courtOfTheGreatOldOnes 4 (Static 6) connectsToAdjacent

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes a) =
    extendRevealed
      a
      [ restricted a 1 (CluesOnThis $ LessThan $ Static 6) $ forced $ RoundEnds #when
      , withTooltip
          "Shuffle this location into the Cosmos, moving each investigator and enemy that was at this location to Court of the Great Old Ones"
          $ restricted
            (proxied (LocationMatcherSource $ LocationWithTrait Void) a)
            1
            Here
            actionAbility
      ]

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg l@(CourtOfTheGreatOldOnes attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs $ max 0 (6 - attrs.clues)
      pure l
    UseThisAbility _ source@(ProxySource (LocationSource lid) (isSource attrs -> True)) 1 -> do
      selectEach (investigatorAt lid) \iid -> moveTo (toAbilitySource source 1) iid attrs
      removeLocation lid
      card <- field Field.LocationCard lid
      shuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) [card]
      pure l
    _ -> CourtOfTheGreatOldOnes <$> liftRunMessage msg attrs
