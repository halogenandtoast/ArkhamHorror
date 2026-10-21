module Arkham.Enemy.Cards.TracesToNowhere.AlbrechtHusserlDesperateGambler (albrechtHusserlDesperateGambler) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype AlbrechtHusserlDesperateGambler = AlbrechtHusserlDesperateGambler EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

albrechtHusserlDesperateGambler :: EnemyCard AlbrechtHusserlDesperateGambler
albrechtHusserlDesperateGambler = enemy AlbrechtHusserlDesperateGambler Cards.albrechtHusserlDesperateGambler

instance HasAbilities AlbrechtHusserlDesperateGambler where
  getAbilities (AlbrechtHusserlDesperateGambler a) =
    extend
      a
      [ culpritShamblerDamage a 1
      , restricted a 2 (OnSameLocation <> (thisExists a EnemyWithAnyDamage)) parleyAction_
      ]

instance RunMessage AlbrechtHusserlDesperateGambler where
  runMessage msg e@(AlbrechtHusserlDesperateGambler attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      nonAttackEnemyDamage Nothing (attrs.ability 1) 1 attrs
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Stories.luckRunningOut
      pure e
    _ -> AlbrechtHusserlDesperateGambler <$> liftRunMessage msg attrs
