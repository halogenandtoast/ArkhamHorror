module Arkham.Homebrew.AgesUnwound.Treacheries.JustBusiness (justBusiness) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Treachery.Import.Lifted

newtype JustBusiness = JustBusiness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justBusiness :: TreacheryCard JustBusiness
justBusiness = treachery JustBusiness Cards.justBusiness

instance RunMessage JustBusiness where
  runMessage msg t@(JustBusiness attrs) = runQueueT $ case msg of
    -- "Revelation - Test [willpower] (3). If you fail, take 2 horror and move to
    -- a new Arkham Streets location."
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      moveToNewArkhamStreetsLocation attrs iid
      pure t
    _ -> JustBusiness <$> liftRunMessage msg attrs
