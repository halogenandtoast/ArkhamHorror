module Arkham.Agenda.Cards.TheDevilOfTheDepths (
  TheDevilOfTheDepths (..),
  theDevilOfTheDepths,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Ocean))

newtype TheDevilOfTheDepths = TheDevilOfTheDepths AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilOfTheDepths :: AgendaCard TheDevilOfTheDepths
theDevilOfTheDepths = agenda (2, A) TheDevilOfTheDepths Cards.theDevilOfTheDepths (Static 9)

instance HasModifiersFor TheDevilOfTheDepths where
  getModifiersFor (TheDevilOfTheDepths a) = do
    modifySelect
      a
      (not_ (InVehicleMatching AnyAsset) <> at_ (LocationWithTrait Ocean))
      [AdditionalCostToEnterMatching (LocationWithTrait Ocean) (ActionCost 2)]

instance HasAbilities TheDevilOfTheDepths where
  getAbilities (TheDevilOfTheDepths a) = [needsAir a 1]

instance RunMessage TheDevilOfTheDepths where
  runMessage msg a@(TheDevilOfTheDepths attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      record TheTideHasGrownStronger
      push R1
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    _ -> TheDevilOfTheDepths <$> liftRunMessage msg attrs
