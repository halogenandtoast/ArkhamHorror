module Arkham.Agenda.Cards.TheInnsmouthConspiracy.ALightInTheFog.TheTideRises (theTideRises) where

import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Enemies
import Arkham.Helpers.Card (findUniqueCard)
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.ALightInTheFog qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.Trait (Trait (FalconPoint))
import Arkham.Zone

newtype TheTideRises = TheTideRises AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRises :: AgendaCard TheTideRises
theTideRises = agenda (3, A) TheTideRises Cards.theTideRises (Static 10)

instance RunMessage TheTideRises where
  runMessage msg a@(TheTideRises attrs) = runQueueT $ case msg of
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
        enemyMoveTo attrs eid upperDepths

      selectEach (LocationWithTrait FalconPoint) removeLocation

      floodBottommost_ 4

      advanceAgendaDeck attrs
      pure a
    _ -> TheTideRises <$> liftRunMessage msg attrs
