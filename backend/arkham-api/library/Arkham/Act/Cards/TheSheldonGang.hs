module Arkham.Act.Cards.TheSheldonGang (theSheldonGang) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheSheldonGang = TheSheldonGang ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSheldonGang :: ActCard TheSheldonGang
theSheldonGang = act (2, A) TheSheldonGang Cards.theSheldonGang Nothing

-- TODO: Sadie gains a granted parley action (spend up to 4 resources; test
-- [willpower]/[combat] (5), -1 difficulty per resource; place clues on Sadie
-- equal to the amount succeeded by). For now the act advances when Sadie has
-- damage or 3 clues, checked at the end of the round.
instance HasAbilities TheSheldonGang where
  getAbilities (TheSheldonGang a) | onSide A a =
    [ restricted
        a
        1
        ( exists
            $ enemyIs Enemies.sadieSheldon
            <> oneOf [EnemyWithDamage (atLeast 1), EnemyWithClues (atLeast 3)]
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]
  getAbilities _ = []

instance RunMessage TheSheldonGang where
  runMessage msg a@(TheSheldonGang attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      hibbs <- selectJust (LocationWithTitle "Hibb's Roadhouse")
      naomi <- getSetAsideCard Enemies.naomiOBannion
      enforcer <- getSetAsideCard Enemies.gangEnforcer
      createEnemyAt_ naomi hibbs
      createEnemyAt_ enforcer hibbs
      -- TODO: should search the encounter deck/discard for the Gang Soldier
      -- copy; for now we create one. Also: if a clue is on Sadie, place 2 damage
      -- on Naomi; move up to 2 clues from Sadie to non-Elite Criminal enemies;
      -- enemies with clues disengage and gain aloof.
      gangSoldier <- genCard Enemies.gangSoldier
      createEnemyAt_ gangSoldier hibbs
      advanceToAct attrs Cards.faceTheMusic A
      pure a
    _ -> TheSheldonGang <$> liftRunMessage msg attrs
