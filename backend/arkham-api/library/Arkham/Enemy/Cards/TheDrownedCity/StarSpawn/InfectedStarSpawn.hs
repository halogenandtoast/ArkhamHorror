module Arkham.Enemy.Cards.TheDrownedCity.StarSpawn.InfectedStarSpawn (infectedStarSpawn) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheDrownedCity.StarSpawn qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype InfectedStarSpawn = InfectedStarSpawn EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infectedStarSpawn :: EnemyCard InfectedStarSpawn
infectedStarSpawn =
  enemyWith
    InfectedStarSpawn
    Cards.infectedStarSpawn
    (preyL .~ Prey (InvestigatorWithHighestSkill #agility UneliminatedInvestigator))

instance HasAbilities InfectedStarSpawn where
  getAbilities (InfectedStarSpawn a) =
    extend
      a
      [ restricted a 1 (prohibit $ getEnemyMetaDefault False a)
          $ forced
          $ EnemyWouldBeDefeated #when (be a)
      , restricted
          a
          2
          (DuringSkillTest $ SkillTestOneOf [WhileAttackingAnEnemy $ be a, WhileEvadingAnEnemy $ be a])
          $ forced
          $ RevealChaosToken #after You #elderthing
      ]

instance RunMessage InfectedStarSpawn where
  runMessage msg e@(InfectedStarSpawn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      readyThis attrs
      pure $ InfectedStarSpawn $ attrs & setMeta True
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure e
    _ -> InfectedStarSpawn <$> liftRunMessage msg attrs
