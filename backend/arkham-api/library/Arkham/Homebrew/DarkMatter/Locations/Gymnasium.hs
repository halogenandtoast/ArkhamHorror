module Arkham.Homebrew.DarkMatter.Locations.Gymnasium (gymnasium) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Gymnasium = Gymnasium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gymnasium :: LocationCard Gymnasium
gymnasium = locationWith Gymnasium Cards.gymnasium 3 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Gymnasium where
  getAbilities (Gymnasium a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyEnters #when (be a) AnyEnemy

instance RunMessage Gymnasium where
  runMessage msg l@(Gymnasium attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> eid) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) eid #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTestTargetedEnemy \eid -> do
        roundModifiers
          (attrs.ability 1)
          iid
          [CannotBeEngagedBy (EnemyWithId eid), CannotBeAttackedBy (EnemyWithId eid)]
      pure l
    _ -> Gymnasium <$> liftRunMessage msg attrs
