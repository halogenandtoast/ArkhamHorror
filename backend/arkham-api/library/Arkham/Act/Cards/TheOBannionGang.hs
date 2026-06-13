module Arkham.Act.Cards.TheOBannionGang (theOBannionGang) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheOBannionGang = TheOBannionGang ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOBannionGang :: ActCard TheOBannionGang
theOBannionGang = act (2, A) TheOBannionGang Cards.theOBannionGang Nothing

-- TODO: Naomi gains a granted parley action (spend up to 4 resources; test
-- [intellect]/[agility] (5), -1 difficulty per resource; place clues on Naomi
-- equal to the amount succeeded by). For now the act advances when Naomi has
-- damage or 3 clues, checked at the end of the round.
instance HasAbilities TheOBannionGang where
  getAbilities (TheOBannionGang a) | onSide A a =
    [ restricted
        a
        1
        ( exists
            $ enemyIs Enemies.naomiOBannion
            <> oneOf [EnemyWithDamage (atLeast 1), EnemyWithClues (atLeast 3)]
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]
  getAbilities _ = []

instance RunMessage TheOBannionGang where
  runMessage msg a@(TheOBannionGang attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      laBellaLuna <- selectJust (LocationWithTitle "La Bella Luna")
      sadie <- getSetAsideCard Enemies.sadieSheldon
      enforcer <- getSetAsideCard Enemies.gangEnforcer
      createEnemyAt_ sadie laBellaLuna
      createEnemyAt_ enforcer laBellaLuna
      -- TODO: should search the encounter deck/discard for the Gang Soldier
      -- copy; for now we create one. Also: if a clue is on Naomi, place 2 damage
      -- on Sadie; move up to 2 clues from Naomi to non-Elite Criminal enemies;
      -- enemies with clues disengage and gain aloof.
      gangSoldier <- genCard Enemies.gangSoldier
      createEnemyAt_ gangSoldier laBellaLuna
      advanceToAct attrs Cards.faceTheMusic A
      pure a
    _ -> TheOBannionGang <$> liftRunMessage msg attrs
