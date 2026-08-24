module Arkham.Homebrew.DarkMatter.Enemies.MiGoStabilizer (miGoStabilizer) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawRandomFacedownCard, yourFacedownCardsAtLeast)
import Arkham.Matcher

newtype MiGoStabilizer = MiGoStabilizer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "Spawn - Location with the most clues. Hunter." (Hunter is printed, so it
-- comes from the card def's keywords.)
miGoStabilizer :: EnemyCard MiGoStabilizer
miGoStabilizer =
  enemy MiGoStabilizer Cards.miGoStabilizer & setSpawnAt (LocationWithMostClues Anywhere)

{- | "Forced - After Mi-Go Stabilizer attacks you: Draw a face-down encounter
card from your threat area."

Having a card to draw is the trigger condition, so it is a 'Criterion': a forced
ability is a mandatory click, and checking the count only in the handler leaves
one on offer after every attack that does nothing.
-}
instance HasAbilities MiGoStabilizer where
  getAbilities (MiGoStabilizer a) =
    extend1 a
      $ restricted a 1 (yourFacedownCardsAtLeast 1)
      $ forced
      $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage MiGoStabilizer where
  runMessage msg e@(MiGoStabilizer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      void $ drawRandomFacedownCard iid
      pure e
    _ -> MiGoStabilizer <$> liftRunMessage msg attrs
