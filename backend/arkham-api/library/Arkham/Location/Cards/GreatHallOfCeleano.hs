module Arkham.Location.Cards.GreatHallOfCeleano (greatHallOfCeleano) where

import Arkham.Ability
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Strategy

newtype GreatHallOfCeleano = GreatHallOfCeleano LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatHallOfCeleano :: LocationCard GreatHallOfCeleano
greatHallOfCeleano = location GreatHallOfCeleano Cards.greatHallOfCeleano 3 (Static 3)

instance HasAbilities GreatHallOfCeleano where
  getAbilities (GreatHallOfCeleano a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ triggered (Moves #after You AnySource Anywhere (be a)) (DoomCost (a.ability 1) (toTarget a) 1)

instance RunMessage GreatHallOfCeleano where
  runMessage msg l@(GreatHallOfCeleano attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      chooseOneM iid do
        for_ investigators \iid' -> do
          deckLabeled iid' $ lookAt iid (attrs.ability 1) iid' [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
        targeting EncounterDeckTarget
          $ lookAt iid (attrs.ability 1) EncounterDeckTarget [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
        targeting ExplorationDeck
          $ lookAt iid (attrs.ability 1) ExplorationDeck [fromTopOfDeck 1] #any (defer attrs IsNotDraw)

      pure l
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseOneM iid $ withI18n do
        labeled' "discard" $ for_ cards (discardCard iid (attrs.ability 1))
        labeled' "continue" nothing
      pure l
    _ -> GreatHallOfCeleano <$> liftRunMessage msg attrs
