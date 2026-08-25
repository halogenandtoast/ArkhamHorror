module Arkham.Homebrew.DarkMatter.Enemies.SpiritOfThan (spiritOfThan) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SpiritOfThan = SpiritOfThan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Prey - Least "Memories" only."
spiritOfThan :: EnemyCard SpiritOfThan
spiritOfThan =
  setOnlyPrey (investigatorWithLeastRecordCount Memories) $ enemy SpiritOfThan Cards.spiritOfThan

-- | "Hunter. Retaliate."
instance HasModifiersFor SpiritOfThan where
  getModifiersFor (SpiritOfThan a) = modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]

{- | "Forced - When Spirit of Than attacks you, it deals +1 horror for every 2 of
your "Memories"." The tally is the attacked investigator's, which retaliate can
make someone other than the engaged investigator.
-}
instance HasAbilities SpiritOfThan where
  getAbilities (SpiritOfThan a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage SpiritOfThan where
  runMessage msg e@(SpiritOfThan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      memories <- getMemories iid
      when (memories >= 2)
        $ enemyAttackModifier (attrs.ability 1) attrs (HorrorDealt $ memories `div` 2)
      pure e
    _ -> SpiritOfThan <$> liftRunMessage msg attrs
