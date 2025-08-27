module Arkham.Story.Cards.DissectedExplorer (dissectedExplorer) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy

newtype DissectedExplorer = DissectedExplorer StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissectedExplorer :: StoryCard DissectedExplorer
dissectedExplorer = story DissectedExplorer Cards.dissectedExplorer

instance RunMessage DissectedExplorer where
  runMessage msg s@(DissectedExplorer attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      search iid attrs iid [fromDeck] (basic $ CardWithTitle "Tekeli-li") (defer attrs IsNotDraw)
      addToVictory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseTargetM iid cards $ putCardOnBottomOfDeck iid TekeliliDeck
      pure s
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      withI18n $ prompt_ iid "noCardsFound"
      pure s
    _ -> DissectedExplorer <$> liftRunMessage msg attrs
