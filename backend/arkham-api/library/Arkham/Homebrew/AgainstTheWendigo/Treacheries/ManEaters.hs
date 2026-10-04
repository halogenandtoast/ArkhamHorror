module Arkham.Homebrew.AgainstTheWendigo.Treacheries.ManEaters (manEaters) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype ManEaters = ManEaters TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manEaters :: TreacheryCard ManEaters
manEaters = treachery ManEaters Cards.manEaters

instance RunMessage ManEaters where
  runMessage msg t@(ManEaters attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ locationIs Locations.swamp) \iid -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      assignDamage iid attrs n
      pure t
    _ -> ManEaters <$> liftRunMessage msg attrs
