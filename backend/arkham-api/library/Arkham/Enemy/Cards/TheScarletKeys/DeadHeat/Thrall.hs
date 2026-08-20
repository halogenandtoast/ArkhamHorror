module Arkham.Enemy.Cards.TheScarletKeys.DeadHeat.Thrall (thrall) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Enemy.CardDefs.TheScarletKeys.DeadHeat qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest.Lifted (evade)
import Arkham.Keyword
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier hiding (EnemyEvade)

newtype Thrall = Thrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thrall :: EnemyCard Thrall
thrall = enemy Thrall Cards.thrall

instance HasAbilities Thrall where
  getAbilities (Thrall a) =
    extend
      a
      [ limited (MaxPer Cards.thrall PerRound 1)
          $ mkAbility a 0
          $ silent
          $ EnemyWouldSpawnAt (be a) Anywhere
      , playerLimit PerTurn
          $ restricted
            a
            1
            ( OnSameLocation
                <> EnemyCriteria
                  (ThisEnemy $ EnemyMatchAll [EnemyWithEvade, EnemyWithoutModifier CannotBeEvaded])
                <> DuringTurn You
            )
          $ FastAbility Free
      ]

instance RunMessage Thrall where
  runMessage msg e@(Thrall attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 0 -> do
      push $ GainSurge (toSource attrs) (toTarget attrs.cardId)
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) attrs (AddKeyword Alert)
      evade sid iid (attrs.ability 1) attrs #agility (EnemyMaybeFieldCalculation attrs.id EnemyEvade)
      pure e
    _ -> Thrall <$> liftRunMessage msg attrs
