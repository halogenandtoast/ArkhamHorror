module Arkham.Homebrew.AgainstTheWendigo.Enemies.BernardEpstein (bernardEpstein) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype BernardEpstein = BernardEpstein EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bernardEpstein :: EnemyCard BernardEpstein
bernardEpstein = enemy BernardEpstein Cards.bernardEpstein & setPrey LowestRemainingHealth

instance HasModifiersFor BernardEpstein where
  getModifiersFor (BernardEpstein a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n]

instance HasAbilities BernardEpstein where
  getAbilities (BernardEpstein a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
      , {- The card is the back of Bernard's Fate, so there is no encounter back to
        discard it to: a defeat takes it out of the game, the way the parley below
        does. @#when@, so it is gone before the defeat would discard it, and
        'silent' because the card prints no Forced for it -- it is a consequence
        of the card, not an effect. -}
        mkAbility a 2 $ silent $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage BernardEpstein where
  runMessage msg e@(BernardEpstein attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 5)
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      removeFromGame attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeFromGame attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> BernardEpstein <$> liftRunMessage msg attrs
