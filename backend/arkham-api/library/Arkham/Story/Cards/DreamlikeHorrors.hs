module Arkham.Story.Cards.DreamlikeHorrors (dreamlikeHorrors) where

import Arkham.Deck qualified as Deck
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Creature))

newtype DreamlikeHorrors = DreamlikeHorrors StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamlikeHorrors :: StoryCard DreamlikeHorrors
dreamlikeHorrors = story DreamlikeHorrors Cards.dreamlikeHorrors

instance RunMessage DreamlikeHorrors where
  runMessage msg s@(DreamlikeHorrors attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      enemies <- select $ at_ (locationIs Locations.skaiRiver) <> EnemyWithTrait Creature <> not_ IsSwarm
      if null enemies
        then discardUntilFirst iid attrs Deck.EncounterDeck (basic $ CardWithTrait Creature)
        else do
          lead <- getLead
          chooseTargetM iid enemies \enemy -> push $ PlaceSwarmCards lead enemy 1

      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      createEnemy_ ec iid
      pure s
    _ -> DreamlikeHorrors <$> liftRunMessage msg attrs
