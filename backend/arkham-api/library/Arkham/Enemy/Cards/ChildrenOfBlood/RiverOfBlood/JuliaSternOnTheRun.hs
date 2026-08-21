module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.JuliaSternOnTheRun (juliaSternOnTheRun) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (reduceDamageTakenTo)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype JuliaSternOnTheRun = JuliaSternOnTheRun EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternOnTheRun :: EnemyCard JuliaSternOnTheRun
juliaSternOnTheRun = enemy JuliaSternOnTheRun Cards.juliaSternOnTheRun

instance HasModifiersFor JuliaSternOnTheRun where
  getModifiersFor (JuliaSternOnTheRun a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier n]

instance HasAbilities JuliaSternOnTheRun where
  getAbilities (JuliaSternOnTheRun a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyWouldTakeDamage #when AnySource (be a)

instance RunMessage JuliaSternOnTheRun where
  runMessage msg e@(JuliaSternOnTheRun attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if Token.countTokens Token.Snare attrs.tokens > 0
        then removeTokens (attrs.ability 1) attrs Token.Snare 1
        else reduceDamageTakenTo attrs 0
      pure e
    _ -> JuliaSternOnTheRun <$> liftRunMessage msg attrs
