module Arkham.Homebrew.DarkMatter.Enemies.SpiritOfThan (spiritOfThan) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype SpiritOfThan = SpiritOfThan EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Prey - Least "Memories" only."
spiritOfThan :: EnemyCard SpiritOfThan
spiritOfThan =
  setOnlyPrey (investigatorWithLeastRecordCount Memories) $ enemy SpiritOfThan Cards.spiritOfThan

{- | "Hunter. Retaliate. / Forced - When Spirit of Than attacks you, it deals +1
horror for every 2 of your "Memories"." Modelled as a standing horror bonus
scaled to the engaged investigator's tally.
-}
instance HasModifiersFor SpiritOfThan where
  getModifiersFor (SpiritOfThan a) = do
    modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Retaliate]
    engaged <- select $ InvestigatorEngagedWith (EnemyWithId a.id)
    for_ engaged \iid -> do
      memories <- getMemories iid
      when (memories >= 2) $ modifySelf a [HorrorDealt $ memories `div` 2]

instance RunMessage SpiritOfThan where
  runMessage msg (SpiritOfThan attrs) = SpiritOfThan <$> runMessage msg attrs
