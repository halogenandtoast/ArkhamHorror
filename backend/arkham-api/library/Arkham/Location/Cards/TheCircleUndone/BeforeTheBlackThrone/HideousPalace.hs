module Arkham.Location.Cards.TheCircleUndone.BeforeTheBlackThrone.HideousPalace (hideousPalace) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.TheCircleUndone.BeforeTheBlackThrone qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Trait (Trait (Void))

newtype HideousPalace = HideousPalace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hideousPalace :: LocationCard HideousPalace
hideousPalace = locationWith HideousPalace Cards.hideousPalace 3 (Static 4) connectsToAdjacent

instance HasAbilities HideousPalace where
  getAbilities (HideousPalace a) =
    extendRevealed
      a
      [ restricted a 1 (CluesOnThis $ lessThan 4) $ forced $ RoundEnds #when
      , scenarioI18n
          $ withI18nTooltip "hideousPalace.ability"
          $ withHighlight a
          $ restricted (proxied (LocationMatcherSource $ withTrait Void) a) 1 Here actionAbility
      ]

instance RunMessage HideousPalace where
  runMessage msg l@(HideousPalace attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs $ max 0 (4 - attrs.clues)
      pure l
    UseThisAbility _ (ProxySource (LocationSource lid) (isSource attrs -> True)) 1 -> do
      investigators <- select $ investigatorAt lid
      card <- field Field.LocationCard lid
      for_ investigators \iid -> moveTo (attrs.ability 1) iid attrs
      removedLocation lid
      shuffleCardsIntoDeck CosmosDeck (only card)
      pure l
    Do (PlaceCosmos _ (is attrs -> True) cloc) -> do
      handleCosmos attrs cloc
      pure l
    _ -> HideousPalace <$> liftRunMessage msg attrs
