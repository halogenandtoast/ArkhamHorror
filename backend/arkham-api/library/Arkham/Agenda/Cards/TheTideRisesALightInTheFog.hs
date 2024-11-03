module Arkham.Agenda.Cards.TheTideRisesALightInTheFog (
  TheTideRisesALightInTheFog (..),
  theTideRisesALightInTheFog,
) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card (findUniqueCard)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype TheTideRisesALightInTheFog = TheTideRisesALightInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRisesALightInTheFog :: AgendaCard TheTideRisesALightInTheFog
theTideRisesALightInTheFog = agenda (3, A) TheTideRisesALightInTheFog Cards.theTideRisesALightInTheFog (Static 10)

instance RunMessage TheTideRisesALightInTheFog where
  runMessage msg a@(TheTideRisesALightInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      oceirosMarsh <- findUniqueCard Enemies.oceirosMarsh
      createEnemyAtLocationMatching_ oceirosMarsh (locationIs Locations.sunkenGrottoUpperDepths)
      advanceAgendaDeck attrs
      pure a
    _ -> TheTideRisesALightInTheFog <$> liftRunMessage msg attrs
