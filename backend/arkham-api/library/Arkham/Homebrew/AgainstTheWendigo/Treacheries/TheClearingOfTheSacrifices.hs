module Arkham.Homebrew.AgainstTheWendigo.Treacheries.TheClearingOfTheSacrifices (
  theClearingOfTheSacrifices,
) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype TheClearingOfTheSacrifices = TheClearingOfTheSacrifices TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theClearingOfTheSacrifices :: TreacheryCard TheClearingOfTheSacrifices
theClearingOfTheSacrifices = treachery TheClearingOfTheSacrifices Cards.theClearingOfTheSacrifices

instance RunMessage TheClearingOfTheSacrifices where
  runMessage msg t@(TheClearingOfTheSacrifices attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ locationIs Locations.siteOfAncientStones) \iid -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    -- "1 horror, or 2 horror if failed by a difference of 2 points or more."
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n -> do
      assignHorror iid attrs (if n >= 2 then 2 else 1)
      pure t
    _ -> TheClearingOfTheSacrifices <$> liftRunMessage msg attrs
