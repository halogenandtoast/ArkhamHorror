module Arkham.Homebrew.AgesUnwound.Acts.BigAndUgly_053 (bigAndUgly_053) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyDamage))
import Arkham.Helpers.Modifiers (ModifierType (CannotAttack), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record, recordCount)
import Arkham.Projection

newtype BigAndUgly_053 = BigAndUgly_053 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The printed clue requirement ("Investigators may spend the requisite number
of clues to advance") is the act's own cost, which gives the engine's
spend-clues-to-advance ability and reports 'AdvancedWithClues'.
-}
bigAndUgly_053 :: ActCard BigAndUgly_053
bigAndUgly_053 =
  act (2, A) BigAndUgly_053 Cards.bigAndUgly_053 (Just $ GroupClueCost (PerPlayer 4) Anywhere)

-- | "Whilst the Hound of Unmaking is undamaged, it cannot make attacks."
instance HasModifiersFor BigAndUgly_053 where
  getModifiersFor (BigAndUgly_053 a) =
    modifySelect
      a
      (enemyIs Enemies.houndOfUnmaking <> EnemyWithDamage (EqualTo $ Static 0))
      [CannotAttack]

-- | "Objective - If the Hound of Unmaking is defeated, advance."
instance HasAbilities BigAndUgly_053 where
  getAbilities (BigAndUgly_053 a) =
    [mkAbility a 1 $ Objective $ forced $ ifEnemyDefeated Enemies.houndOfUnmaking]

instance RunMessage BigAndUgly_053 where
  runMessage msg a@(BigAndUgly_053 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ advanceMode | aid == actId attrs && onSide B attrs -> do
      hound <- selectOne $ enemyIs Enemies.houndOfUnmaking
      if advanceMode == AdvancedWithClues
        then do
          {- "If you spent clues to advance: ... record that the investigators
          repelled the Hound of Unmaking. Next to this, record the time and the
          amount of damage on the Hound of Unmaking. Remove the Hound of Unmaking
          from the game."

          The damage is read before the removal is queued -- a field read happens
          now, the 'removeFromGame' only when the queue reaches it, but keeping
          them in this order is what makes that obvious. Zero is recorded as
          zero: the act's own "whilst the Hound is undamaged it cannot make
          attacks" makes an undamaged repel a real outcome, and Scenario VI's
          "if an amount of damage is recorded" then places none. -}
          record TheInvestigatorsRepelledTheHoundOfUnmaking
          recordTheTimeFor TheInvestigatorsRepelledTheHoundOfUnmaking
          recordCount DamageOnTheHoundOfUnmaking
            . fromMaybe 0
            =<< traverse (field EnemyDamage) hound

          for_ hound removeFromGame
        else do
          {- "If you defeated the Hound of Unmaking: ... record that the
          investigators put down the Hound of Unmaking. Next to this, record the
          time." Nothing to remove -- it is already in the victory display. -}
          record TheInvestigatorsPutDownTheHoundOfUnmaking
          recordTheTimeFor TheInvestigatorsPutDownTheHoundOfUnmaking

      {- "However you advanced: Put the set aside Ritual Circle (Gateway to the
      Past) location into play. Advance to Act 3a - Breaking the Circle." -}
      placeSetAsideLocation_ Locations.ritualCircle_225
      advanceToAct attrs Cards.breakingTheCircle A
      pure a
    _ -> BigAndUgly_053 <$> liftRunMessage msg attrs
