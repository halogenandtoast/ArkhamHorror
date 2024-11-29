module Arkham.Story.Cards.EvilWithin (EvilWithin (..), evilWithin) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait (Trait (Eidolon))

newtype EvilWithin = EvilWithin StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

evilWithin :: StoryCard EvilWithin
evilWithin = story EvilWithin Cards.evilWithin

instance RunMessage EvilWithin where
  runMessage msg s@(EvilWithin attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      eidolons <- select $ EnemyWithTrait Eidolon <> NonEliteEnemy
      chooseTargetM iid eidolons $ toDiscardBy iid attrs
      addToVictory attrs
      pure s
    _ -> EvilWithin <$> liftRunMessage msg attrs
