module Arkham.Story.Cards.UnspeakableAbomination (unspeakableAbomination) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Query (getSetAsideCardMaybe)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype UnspeakableAbomination = UnspeakableAbomination StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unspeakableAbomination :: StoryCard UnspeakableAbomination
unspeakableAbomination = story UnspeakableAbomination Cards.unspeakableAbomination & persistStory

tyrthrha :: EnemyMatcher
tyrthrha = enemyIs Enemies.tyrthrha

instance HasAbilities UnspeakableAbomination where
  getAbilities (UnspeakableAbomination a) =
    guard a.flipped
      *> [ noAOO
             $ restricted a 1 (youExist $ at_ (LocationWithEnemy tyrthrha))
             $ actionAbilityWithCost (ClueCost (Static 1))
         , mkAbility a 2 $ Objective $ forced $ IfEnemyDefeated #after Anyone ByAny tyrthrha
         ]

instance RunMessage UnspeakableAbomination where
  runMessage msg s@(UnspeakableAbomination attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure s
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let damage = if n >= 3 then 3 else 2
      selectForMaybeM (enemyIs Enemies.tyrthrha)
        $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) damage
      pure s
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      addToVictory iid attrs
      pure s
    FlipThis (isTarget attrs -> True) -> do
      withMatch (locationIs Locations.tindalos) \tindalos -> do
        whenJustM (getSetAsideCardMaybe Enemies.tyrthrha) (`createEnemyAt_` tindalos)
      flippedOver attrs
      pure $ UnspeakableAbomination $ attrs & flippedL .~ True
    _ -> UnspeakableAbomination <$> liftRunMessage msg attrs
