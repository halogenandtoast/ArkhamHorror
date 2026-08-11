module Arkham.Homebrew.DarkMatter.Enemies.MartianCrab (martianCrab) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Mars)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype MartianCrab = MartianCrab EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Spawn - Any [[Mars]] location."
martianCrab :: EnemyCard MartianCrab
martianCrab = enemy MartianCrab Cards.martianCrab & setSpawnAt (LocationWithTrait Mars)

-- | "Massive."
instance HasModifiersFor MartianCrab where
  getModifiersFor (MartianCrab a) = modifySelf a [AddKeyword Keyword.Massive]

{- | "[reaction] After you successfully evade Martian Crab, spend 1 clue: Deal 2
damage to it instead of exhausting it."
-}
instance HasAbilities MartianCrab where
  getAbilities (MartianCrab a) =
    extend1 a
      $ restricted a 1 (youExist $ InvestigatorWithClues $ atLeast 1)
      $ triggered (EnemyEvaded #after You (be a)) (ClueCost $ Static 1)

instance RunMessage MartianCrab where
  runMessage msg e@(MartianCrab attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      nonAttackEnemyDamage (Just iid) (attrs.ability 1) 2 attrs.id
      pure e
    _ -> MartianCrab <$> liftRunMessage msg attrs
