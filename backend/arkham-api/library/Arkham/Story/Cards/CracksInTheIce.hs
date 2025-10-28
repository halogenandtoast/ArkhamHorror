module Arkham.Story.Cards.CracksInTheIce (cracksInTheIce) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy

newtype CracksInTheIce = CracksInTheIce StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cracksInTheIce :: StoryCard CracksInTheIce
cracksInTheIce = story CracksInTheIce Cards.cracksInTheIce

instance RunMessage CracksInTheIce where
  runMessage msg s@(CracksInTheIce attrs) = runQueueT $ case msg of
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
    _ -> CracksInTheIce <$> liftRunMessage msg attrs
