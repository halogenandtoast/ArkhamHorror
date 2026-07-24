module Arkham.Enemy.Cards.CoralStarSpawn (coralStarSpawn) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype CoralStarSpawn = CoralStarSpawn EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coralStarSpawn :: EnemyCard CoralStarSpawn
coralStarSpawn =
  enemyWith
    CoralStarSpawn
    Cards.coralStarSpawn
    (preyL .~ Prey (InvestigatorWithHighestSkill #willpower UneliminatedInvestigator))

instance HasModifiersFor CoralStarSpawn where
  getModifiersFor (CoralStarSpawn a) =
    modifySelfWhen a (a.damage > 0) [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]

instance HasAbilities CoralStarSpawn where
  getAbilities (CoralStarSpawn a) =
    extend1 a
      $ restricted
        a
        1
        (DuringSkillTest $ SkillTestOneOf [WhileAttackingAnEnemy $ be a, WhileEvadingAnEnemy $ be a])
      $ forced
      $ RevealChaosToken #after You #elderthing

instance RunMessage CoralStarSpawn where
  runMessage msg e@(CoralStarSpawn attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> CoralStarSpawn <$> liftRunMessage msg attrs
