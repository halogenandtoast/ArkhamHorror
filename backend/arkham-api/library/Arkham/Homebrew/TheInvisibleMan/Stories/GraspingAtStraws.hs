{- | "If Griffin's fight value is a set number, resolve the attack, card, or
ability. Griffin gets +1 fight while resolving its effects.

Otherwise, if you are attacking, this attack automatically fails. If you played
a card or activated an ability, cancel each of its effects.

In either case, shuffle Grasping at Straws back into the Invisible Combat
deck."

"Griffin's fight value is a set number" is act 1d's memory, not something read
off the enemy -- his printed fight is @?@ until that act pins it to 3.
-}
module Arkham.Homebrew.TheInvisibleMan.Stories.GraspingAtStraws (graspingAtStraws) where

import Arkham.Helpers.Modifiers (ModifierType (EnemyFight))
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Story.Import.Lifted

newtype GraspingAtStraws = GraspingAtStraws StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graspingAtStraws :: StoryCard GraspingAtStraws
graspingAtStraws = story GraspingAtStraws Cards.graspingAtStraws

instance RunMessage GraspingAtStraws where
  runMessage msg s@(GraspingAtStraws attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      attempt <- attackOnGriffin attrs iid
      fightIsSet <- griffinsFightIsASetNumber
      if fightIsSet
        then griffinGetsWhileResolving attrs attempt (EnemyFight 1)
        else theAttackFails attrs attempt
      shuffleBackIntoCombat attrs
      pure s
    _ -> GraspingAtStraws <$> liftRunMessage msg attrs
