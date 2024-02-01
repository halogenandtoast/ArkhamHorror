module Arkham.Agenda.Cards.HospitalOfHorrors (
  HospitalOfHorrors (..),
  hospitalOfHorrors,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Scenarios.WakingNightmare.Helpers

newtype HospitalOfHorrors = HospitalOfHorrors AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hospitalOfHorrors :: AgendaCard HospitalOfHorrors
hospitalOfHorrors = agenda (3, A) HospitalOfHorrors Cards.hospitalOfHorrors (Static 8)

instance HasModifiersFor HospitalOfHorrors where
  getModifiersFor (EnemyTarget eid) (HospitalOfHorrors attrs) = do
    atInfested <- eid <=~> EnemyAt InfestedLocation
    pure $ toModifiers attrs $ guard atInfested *> [EnemyFight 1, EnemyEvade 1]
  getModifiersFor _ _ = pure []

instance HasAbilities HospitalOfHorrors where
  getAbilities (HospitalOfHorrors attrs) =
    [mkAbility attrs 1 $ ForcedAbility $ PhaseEnds #when #mythos]

instance RunMessage HospitalOfHorrors where
  runMessage msg a@(HospitalOfHorrors attrs) =
    case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        pushM makeInfestationTest
        pure a
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        pushAll [RecordCount StepsOfTheBridge 8, R4]
        pure a
      _ -> HospitalOfHorrors <$> runMessage msg attrs
