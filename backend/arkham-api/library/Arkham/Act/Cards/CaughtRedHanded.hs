module Arkham.Act.Cards.CaughtRedHanded (caughtRedHanded) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CaughtRedHanded = CaughtRedHanded ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caughtRedHanded :: ActCard CaughtRedHanded
caughtRedHanded = act (4, A) CaughtRedHanded Cards.caughtRedHanded Nothing

instance HasAbilities CaughtRedHanded where
  getAbilities = actAbilities \a ->
    [ skillTestAbility
        $ restricted
          (proxied (EnemyMatcherSource $ enemyIs Enemies.theRedGlovedManShroudedInMystery) a)
          1
          OnSameLocation
          parleyAction_
    , mkAbility a 2
        $ Objective
        $ forced
        $ oneOf
          [ ifEnemyDefeated Enemies.theRedGlovedManShroudedInMystery
          , AnyWindowIfEnemy
              $ enemyIs Enemies.theRedGlovedManShroudedInMystery
              <> EnemyWithTokens (PerPlayer 1) #resource
          ]
    ]

instance RunMessage CaughtRedHanded where
  runMessage msg a@(CaughtRedHanded attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (AbilitySource p 1) iid [#willpower, #intellect] (Fixed 5)
      pure a
    PassedThisSkillTest _iid source@(isProxyAbilitySource attrs 1 -> True) -> do
      mTheRedGlovedMan <- selectOne $ enemyIs Enemies.theRedGlovedManShroudedInMystery
      for_ mTheRedGlovedMan $ placeTokensOn source #resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      parleyed <-
        selectAny
          $ enemyIs Enemies.theRedGlovedManShroudedInMystery
          <> EnemyWithTokens (PerPlayer 1) #resource
      selectOne (InPlayEnemy $ enemyIs Enemies.theRedGlovedManShroudedInMystery)
        >>= traverse_ addToVictory_
      push $ if parleyed then R2 else R1
      pure a
    _ -> CaughtRedHanded <$> liftRunMessage msg attrs
