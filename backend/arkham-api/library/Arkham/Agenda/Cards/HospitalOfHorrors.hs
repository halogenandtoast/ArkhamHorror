module Arkham.Agenda.Cards.HospitalOfHorrors (HospitalOfHorrors (..), hospitalOfHorrors) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype HospitalOfHorrors = HospitalOfHorrors AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hospitalOfHorrors :: AgendaCard HospitalOfHorrors
hospitalOfHorrors = agenda (3, A) HospitalOfHorrors Cards.hospitalOfHorrors (Static 8)

instance HasModifiersFor HospitalOfHorrors where
  getModifiersFor (HospitalOfHorrors attrs) =
    modifySelect attrs (EnemyAt InfestedLocation) [EnemyFight 1, EnemyEvade 1]

instance HasAbilities HospitalOfHorrors where
  getAbilities (HospitalOfHorrors attrs) =
    [mkAbility attrs 1 $ forced $ PhaseEnds #when #mythos]

instance RunMessage HospitalOfHorrors where
  runMessage msg a@(HospitalOfHorrors attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      makeInfestationTest
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      recordCount StepsOfTheBridge 8
      push R4
      pure a
    _ -> HospitalOfHorrors <$> liftRunMessage msg attrs
