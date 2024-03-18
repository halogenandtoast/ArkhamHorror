module Arkham.Story.Cards.SpiderInfestedWaters (SpiderInfestedWaters (..), spiderInfestedWaters) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Spider))

newtype SpiderInfestedWaters = SpiderInfestedWaters StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiderInfestedWaters :: StoryCard SpiderInfestedWaters
spiderInfestedWaters = story SpiderInfestedWaters Cards.spiderInfestedWaters

instance RunMessage SpiderInfestedWaters where
  runMessage msg s@(SpiderInfestedWaters attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      shuffleEncounterDiscardBackIn
      push
        $ DiscardUntilFirst
          iid
          (toSource attrs)
          Deck.EncounterDeck
          (basic $ CardWithTrait Spider <> CardWithType EnemyType)
      pure s
    RequestedEncounterCard (isSource attrs -> True) _ (Just ec) -> do
      seaOfPitch <- selectJust $ locationIs Locations.seaOfPitch_262
      spider <- createEnemyAt (toCard ec) seaOfPitch
      placeClues attrs spider 1
      pure s
    _ -> SpiderInfestedWaters <$> lift (runMessage msg attrs)
