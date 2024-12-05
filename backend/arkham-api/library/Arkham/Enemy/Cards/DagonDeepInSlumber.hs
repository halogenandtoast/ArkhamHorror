module Arkham.Enemy.Cards.DagonDeepInSlumber (dagonDeepInSlumber, DagonDeepInSlumber (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Key
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype DagonDeepInSlumber = DagonDeepInSlumber EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dagonDeepInSlumber :: EnemyCard DagonDeepInSlumber
dagonDeepInSlumber =
  enemyWith DagonDeepInSlumber Cards.dagonDeepInSlumber (0, Static 1, 0) (0, 0)
    $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor DagonDeepInSlumber where
  getModifiersFor (DagonDeepInSlumber a) = modifySelf a [Omnipotent]

instance HasAbilities DagonDeepInSlumber where
  getAbilities (DagonDeepInSlumber a) =
    [ restricted a 1 OnSameLocation
        $ forced
        $ SkillTestResult #after You AnySkillTest #failure
    , restricted a 2 OnSameLocation
        $ actionAbilityWithCost
          (OrCost $ map SpendKeyCost [BlackKey, RedKey, GreenKey, BlueKey, WhiteKey, YellowKey, PurpleKey])
    ]

instance RunMessage DagonDeepInSlumber where
  runMessage msg e@(DagonDeepInSlumber attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      n <- getGameValue (StaticWithPerPlayer 2 1)
      when (attrs.resources >= n) $ flipOver iid attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeTokens (attrs.ability 1) attrs #resource 1
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      awakened <- genCard Cards.dagonAwakenedAndEnraged
      push $ ReplaceEnemy attrs.id awakened Swap
      pure e
    EnemyCheckEngagement eid | eid == attrs.id -> pure e
    _ -> DagonDeepInSlumber <$> liftRunMessage msg attrs
