module Arkham.Homebrew.DarkMatter.Enemies.MiGoStabilizer (miGoStabilizer) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawFacedownCard, getFacedownCards)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype MiGoStabilizer = MiGoStabilizer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- "Spawn - Location with the most clues. Hunter."
miGoStabilizer :: EnemyCard MiGoStabilizer
miGoStabilizer =
  enemy MiGoStabilizer Cards.miGoStabilizer & setSpawnAt (LocationWithMostClues Anywhere)

instance HasModifiersFor MiGoStabilizer where
  getModifiersFor (MiGoStabilizer a) = modifySelf a [AddKeyword Keyword.Hunter]

-- "Forced - After Mi-Go Stabilizer attacks you: Draw a face-down encounter card
-- from your threat area."
instance HasAbilities MiGoStabilizer where
  getAbilities (MiGoStabilizer a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage MiGoStabilizer where
  runMessage msg e@(MiGoStabilizer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getFacedownCards iid >>= traverse_ (drawFacedownCard iid) . take 1
      pure e
    _ -> MiGoStabilizer <$> liftRunMessage msg attrs
