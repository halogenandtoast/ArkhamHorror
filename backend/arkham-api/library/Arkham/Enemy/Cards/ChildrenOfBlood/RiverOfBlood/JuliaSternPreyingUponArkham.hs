module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.JuliaSternPreyingUponArkham (juliaSternPreyingUponArkham) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (reduceDamageTakenTo)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype JuliaSternPreyingUponArkham = JuliaSternPreyingUponArkham EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternPreyingUponArkham :: EnemyCard JuliaSternPreyingUponArkham
juliaSternPreyingUponArkham = enemy JuliaSternPreyingUponArkham Cards.juliaSternPreyingUponArkham

instance HasModifiersFor JuliaSternPreyingUponArkham where
  getModifiersFor (JuliaSternPreyingUponArkham a) = do
    n <- getPlayerCount
    modifySelf a [HealthModifier (2 * n)]

instance HasAbilities JuliaSternPreyingUponArkham where
  getAbilities (JuliaSternPreyingUponArkham a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyWouldTakeDamage #when AnySource (be a)

instance RunMessage JuliaSternPreyingUponArkham where
  runMessage msg e@(JuliaSternPreyingUponArkham attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      if Token.countTokens Token.Snare attrs.tokens > 0
        then removeTokens (attrs.ability 1) attrs Token.Snare 1
        else reduceDamageTakenTo attrs 0
      pure e
    _ -> JuliaSternPreyingUponArkham <$> liftRunMessage msg attrs
