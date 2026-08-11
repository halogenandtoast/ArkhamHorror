module Arkham.Homebrew.DarkMatter.Enemies.DaemonOfNis (daemonOfNis) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype DaemonOfNis = DaemonOfNis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | "Prey - Most "Memories" only."
daemonOfNis :: EnemyCard DaemonOfNis
daemonOfNis =
  setOnlyPrey (investigatorWithMostRecordCount Memories) $ enemy DaemonOfNis Cards.daemonOfNis

-- | "Hunter. Alert."
instance HasModifiersFor DaemonOfNis where
  getModifiersFor (DaemonOfNis a) = modifySelf a [AddKeyword Keyword.Hunter, AddKeyword Keyword.Alert]

{- | "Forced - When Daemon of Nis attacks you: Cross out 1 tally mark next to your
"Memories"."
-}
instance HasAbilities DaemonOfNis where
  getAbilities (DaemonOfNis a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage DaemonOfNis where
  runMessage msg e@(DaemonOfNis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      crossOffMemories iid 1
      pure e
    _ -> DaemonOfNis <$> liftRunMessage msg attrs
