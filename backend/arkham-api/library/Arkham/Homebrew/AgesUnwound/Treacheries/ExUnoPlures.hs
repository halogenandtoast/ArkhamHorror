module Arkham.Homebrew.AgesUnwound.Treacheries.ExUnoPlures (exUnoPlures) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Treachery.Import.Lifted

newtype ExUnoPlures = ExUnoPlures TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exUnoPlures :: TreacheryCard ExUnoPlures
exUnoPlures = treachery ExUnoPlures Cards.exUnoPlures

{- | "Revelation - Test [agility] (3). For each point you fail by, spawn a copy of
The Myriad Gentleman engaged with you."

Each copy is the same unconditional spawn, so the count goes in as one amount --
the two-part @doStep@/@doNextStep@ idiom is only needed when every iteration has
to stop and ask the player something.
-}
instance RunMessage ExUnoPlures where
  runMessage msg t@(ExUnoPlures attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      spawnMyriadCopiesEngagedWith iid n
      pure t
    _ -> ExUnoPlures <$> liftRunMessage msg attrs
