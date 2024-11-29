module Arkham.Story.Cards.SomberRemains (SomberRemains (..), somberRemains) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Strategy
import Arkham.Target

newtype SomberRemains = SomberRemains StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somberRemains :: StoryCard SomberRemains
somberRemains = story SomberRemains Cards.somberRemains

instance RunMessage SomberRemains where
  runMessage msg s@(SomberRemains attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ CardWithTitle "Tekeli-li") (defer attrs IsNotDraw)
      addToVictory attrs
      pure s
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      chooseOneAtATimeM iid $ targets cards $ putCardOnBottomOfDeck iid TekeliliDeck
      pure s
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      prompt iid "No Cards Found" []
      pure s
    _ -> SomberRemains <$> liftRunMessage msg attrs
