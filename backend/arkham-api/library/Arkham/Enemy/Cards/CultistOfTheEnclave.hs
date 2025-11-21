module Arkham.Enemy.Cards.CultistOfTheEnclave (cultistOfTheEnclave) where

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype CultistOfTheEnclave = CultistOfTheEnclave EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cultistOfTheEnclave :: EnemyCard CultistOfTheEnclave
cultistOfTheEnclave =
  enemy CultistOfTheEnclave Cards.cultistOfTheEnclave (3, Static 2, 3) (1, 0)
    & setSpawnAt "Basement"

instance HasAbilities CultistOfTheEnclave where
  getAbilities (CultistOfTheEnclave a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage CultistOfTheEnclave where
  runMessage msg e@(CultistOfTheEnclave attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      requestChaosTokens iid (attrs.ability 1) 1
      pure e
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) (map chaosTokenFace -> tokens) -> do
      continue_ iid
      resetChaosTokens (attrs.ability 1)
      when (any (`elem` tokens) [#skull, #cultist, #tablet, #elderthing, #autofail]) $ do
        agenda <- selectJust AnyAgenda
        placeDoom (attrs.ability 1) agenda 1
      pure e
    _ -> CultistOfTheEnclave <$> liftRunMessage msg attrs
