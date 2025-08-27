module Arkham.Story.Cards.BloodyEvidence (bloodyEvidence) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy

newtype BloodyEvidence = BloodyEvidence StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodyEvidence :: StoryCard BloodyEvidence
bloodyEvidence = story BloodyEvidence Cards.bloodyEvidence

instance RunMessage BloodyEvidence where
  runMessage msg s@(BloodyEvidence attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ CardWithTitle "Tekeli-li") (defer attrs IsNotDraw)
      addToVictory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseOneAtATimeM iid $ targets cards $ putCardOnBottomOfDeck iid TekeliliDeck
      pure s
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      withI18n $ prompt_ iid "noCardsFound"
      pure s
    _ -> BloodyEvidence <$> liftRunMessage msg attrs
