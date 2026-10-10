module Arkham.Homebrew.AgesUnwound.Treacheries.WithoutHomeOrIdentity (withoutHomeOrIdentity) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WithoutHomeOrIdentity = WithoutHomeOrIdentity TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

withoutHomeOrIdentity :: TreacheryCard WithoutHomeOrIdentity
withoutHomeOrIdentity = treachery WithoutHomeOrIdentity Cards.withoutHomeOrIdentity

{- | "__Revelation__ - Test [agility] (3). If you fail, lose 1 resource for each
point you fail by, and take 1 damage."

The damage is flat, the resource loss scales -- and both land on a single
failure, so there is no per-point choice to step through.
-}
instance RunMessage WithoutHomeOrIdentity where
  runMessage msg t@(WithoutHomeOrIdentity attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      loseResources iid attrs n
      assignDamage iid attrs 1
      pure t
    _ -> WithoutHomeOrIdentity <$> liftRunMessage msg attrs
