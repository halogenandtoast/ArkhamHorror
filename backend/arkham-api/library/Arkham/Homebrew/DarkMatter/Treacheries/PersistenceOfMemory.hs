module Arkham.Homebrew.DarkMatter.Treacheries.PersistenceOfMemory (persistenceOfMemory) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (crossOffMemories)
import Arkham.Treachery.Import.Lifted

newtype PersistenceOfMemory = PersistenceOfMemory TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persistenceOfMemory :: TreacheryCard PersistenceOfMemory
persistenceOfMemory = treachery PersistenceOfMemory Cards.persistenceOfMemory

{- | "Revelation - Test [agility] (3). If you fail, take 1 horror for each point
you fail by and cross out 1 tally mark next to your 'Memories'."
-}
instance RunMessage PersistenceOfMemory where
  runMessage msg t@(PersistenceOfMemory attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs n
      crossOffMemories iid 1
      pure t
    _ -> PersistenceOfMemory <$> liftRunMessage msg attrs
