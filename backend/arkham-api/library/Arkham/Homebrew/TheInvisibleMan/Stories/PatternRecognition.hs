{- | "If Griffin's fight value is a set number, resolve the attack, card, or
ability.

Otherwise, if you are attacking, this attack automatically fails. If you played
a card or activated an ability, cancel each of its effects.

In either case, shuffle Pattern Recognition back into the Invisible Combat
deck."

Word for word Shot in the Dark; the Invisible Combat deck carries two copies of
the same answer under two names.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.PatternRecognition (patternRecognition) where

import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype PatternRecognition = PatternRecognition StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

patternRecognition :: StoryCard PatternRecognition
patternRecognition = story PatternRecognition Cards.patternRecognition

instance RunMessage PatternRecognition where
  runMessage msg s@(PatternRecognition attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      unlessM griffinsFightIsASetNumber $ theAttackFails attrs attempt
      shuffleBackIntoCombat attrs
      pure s
    _ -> PatternRecognition <$> liftRunMessage msg attrs
