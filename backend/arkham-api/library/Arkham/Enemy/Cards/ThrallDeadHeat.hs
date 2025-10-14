module Arkham.Enemy.Cards.ThrallDeadHeat (thrallDeadHeat) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest.Lifted (evade)
import Arkham.Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier hiding (EnemyEvade)

newtype ThrallDeadHeat = ThrallDeadHeat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thrallDeadHeat :: EnemyCard ThrallDeadHeat
thrallDeadHeat = enemy ThrallDeadHeat Cards.thrallDeadHeat (2, Static 2, 1) (1, 0)

instance HasAbilities ThrallDeadHeat where
  getAbilities (ThrallDeadHeat a) =
    extend
      a
      [ limited (MaxPer Cards.thrallDeadHeat PerRound 1)
          $ mkAbility a 0
          $ silent
          $ EnemyWouldSpawnAt (be a) Anywhere
      , playerLimit PerTurn $ restricted a 1 (EvadeCriteria <> DuringTurn You) $ FastAbility Free
      ]

instance RunMessage ThrallDeadHeat where
  runMessage msg e@(ThrallDeadHeat attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 0 -> do
      push $ GainSurge (toSource attrs) (toTarget attrs.cardId)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) attrs (AddKeyword Alert)
      evade sid iid (attrs.ability 1) attrs #agility (EnemyMaybeFieldCalculation attrs.id EnemyEvade)
      pure e
    _ -> ThrallDeadHeat <$> liftRunMessage msg attrs
