module Arkham.Homebrew.DarkMatter.Locations.Gymnasium (gymnasium) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTestTarget)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype Gymnasium = Gymnasium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gymnasium :: LocationCard Gymnasium
gymnasium = location Gymnasium Cards.gymnasium 3 (PerPlayer 1)

{- | "[reaction] When an enemy would enter this location: Test [agility] (3). If
you succeed, until the end of the round, that enemy cannot engage or attack you."
-}
instance HasAbilities Gymnasium where
  getAbilities (Gymnasium a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyMoves #when (be a) AnyEnemy

getEnteringEnemy :: [Window] -> Maybe EnemyId
getEnteringEnemy = \case
  (windowType -> Window.EnemyWouldMove eid _ _ _) : _ -> Just eid
  _ : rest -> getEnteringEnemy rest
  [] -> Nothing

instance RunMessage Gymnasium where
  runMessage msg l@(Gymnasium attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnteringEnemy -> Just eid) _ -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) (EnemyTarget eid) #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTestTarget \case
        EnemyTarget eid ->
          roundModifiers
            (attrs.ability 1)
            iid
            [CannotBeEngagedBy (EnemyWithId eid), CannotBeAttackedBy (EnemyWithId eid)]
        _ -> pure ()
      pure l
    _ -> Gymnasium <$> liftRunMessage msg attrs
