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
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Movement
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.Trait (Trait (FalconPoint))
import Arkham.Zone

newtype TheTideRisesALightInTheFog = TheTideRisesALightInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRisesALightInTheFog :: AgendaCard TheTideRisesALightInTheFog
theTideRisesALightInTheFog = agenda (3, A) TheTideRisesALightInTheFog Cards.theTideRisesALightInTheFog (Static 10)

instance RunMessage TheTideRisesALightInTheFog where
  runMessage msg a@(TheTideRisesALightInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectOne (OutOfPlayEnemy VictoryDisplayZone $ enemyIs Enemies.oceirosMarsh) >>= \case
        Just oceirosMarsh -> place oceirosMarsh =<< selectJust (locationIs Locations.sunkenGrottoUpperDepths)
        Nothing -> do
          oceirosMarsh <- findUniqueCard Enemies.oceirosMarsh
          createEnemyAtLocationMatching_ oceirosMarsh (locationIs Locations.sunkenGrottoUpperDepths)

      upperDepths <- selectJust $ locationIs Locations.sunkenGrottoUpperDepths
      selectEach (InvestigatorAt (LocationWithTrait FalconPoint)) \iid -> do
        moveTo_ attrs iid upperDepths

      selectEach (#unengaged <> EnemyAt (LocationWithTrait FalconPoint)) \eid -> do
        push $ Move $ move attrs eid upperDepths

      selectEach (LocationWithTrait FalconPoint) removeLocation

      floodBottommost_ 4

      advanceAgendaDeck attrs
      pure a
    _ -> TheTideRisesALightInTheFog <$> liftRunMessage msg attrs
