module Arkham.Story.Cards.DissectedExplorer (DissectedExplorer (..), dissectedExplorer) where

import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Target

newtype DissectedExplorer = DissectedExplorer StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissectedExplorer :: StoryCard DissectedExplorer
dissectedExplorer = story DissectedExplorer Cards.dissectedExplorer

instance RunMessage DissectedExplorer where
  runMessage msg s@(DissectedExplorer attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      focusCards [toCard attrs] \unfocus -> do
        continue iid [unfocus]
        search iid attrs iid [fromDeck] (basic $ CardWithTitle "Tekeli-li") (defer attrs IsNotDraw)
        addToVictory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseOneAtATimeM iid $ targets cards $ putCardOnBottomOfDeck iid TekeliliDeck
      pure s
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      prompt iid "No Cards Found" []
      pure s
    _ -> DissectedExplorer <$> liftRunMessage msg attrs
