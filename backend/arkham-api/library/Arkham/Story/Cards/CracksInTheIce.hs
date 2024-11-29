module Arkham.Story.Cards.CracksInTheIce (CracksInTheIce (..), cracksInTheIce) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Target

newtype CracksInTheIce = CracksInTheIce StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cracksInTheIce :: StoryCard CracksInTheIce
cracksInTheIce = story CracksInTheIce Cards.cracksInTheIce

instance RunMessage CracksInTheIce where
  runMessage msg s@(CracksInTheIce attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      search iid attrs iid [fromDeck] (basic $ CardWithTitle "Tekeli-li") (defer attrs IsNotDraw)
      addToVictory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseOneM iid $ targets cards $ putCardOnBottomOfDeck iid TekeliliDeck
      pure s
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      prompt iid "No Cards Found" []
      pure s
    _ -> CracksInTheIce <$> liftRunMessage msg attrs
