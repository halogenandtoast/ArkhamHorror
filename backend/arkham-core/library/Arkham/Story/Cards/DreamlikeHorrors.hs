module Arkham.Story.Cards.DreamlikeHorrors (DreamlikeHorrors (..), dreamlikeHorrors) where

import Arkham.Deck qualified as Deck
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Creature))

newtype DreamlikeHorrors = DreamlikeHorrors StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamlikeHorrors :: StoryCard DreamlikeHorrors
dreamlikeHorrors = story DreamlikeHorrors Cards.dreamlikeHorrors

instance RunMessage DreamlikeHorrors where
  runMessage msg s@(DreamlikeHorrors attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      enemies <-
        selectList $ at_ (locationIs Locations.skaiRiver) <> EnemyWithTrait Creature <> not_ IsSwarm
      if null enemies
        then push $ DiscardUntilFirst iid (toSource attrs) Deck.EncounterDeck (basic $ CardWithTrait Creature)
        else do
          player <- getPlayer iid
          lead <- getLead
          push $ chooseOne player [targetLabel enemy [PlaceSwarmCards lead enemy 1] | enemy <- enemies]

      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      pushM $ createEnemy ec iid
      pure s
    _ -> DreamlikeHorrors <$> runMessage msg attrs
