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
import Arkham.Movement
import Arkham.Trait (Trait (FalconPoint))

newtype TheTideRisesALightInTheFog = TheTideRisesALightInTheFog AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTideRisesALightInTheFog :: AgendaCard TheTideRisesALightInTheFog
theTideRisesALightInTheFog = agenda (3, A) TheTideRisesALightInTheFog Cards.theTideRisesALightInTheFog (Static 10)

-- Remove each Falcon Point location from the game (or add them to the victory display if they have Victory X and no clues on them). Move each enemy and investigator at those locations to Sunken Grotto (Upper Depths). Each other card at those locations is discarded.

-- Find the 4 bottommost locations that can have their flood levels increased. Increase each of their flood levels.

instance RunMessage TheTideRisesALightInTheFog where
  runMessage msg a@(TheTideRisesALightInTheFog attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      oceirosMarsh <- findUniqueCard Enemies.oceirosMarsh
      upperDepths <- selectJust $ locationIs Locations.sunkenGrottoUpperDepths
      createEnemyAtLocationMatching_ oceirosMarsh (locationIs Locations.sunkenGrottoUpperDepths)
      selectEach (InvestigatorAt (LocationWithTrait FalconPoint)) \iid -> do
        moveTo_ attrs iid upperDepths

      selectEach (#unengaged <> EnemyAt (LocationWithTrait FalconPoint)) \eid -> do
        push $ Move $ move attrs eid upperDepths

      selectEach (LocationWithTrait FalconPoint) removeLocation
      advanceAgendaDeck attrs
      pure a
    _ -> TheTideRisesALightInTheFog <$> liftRunMessage msg attrs
