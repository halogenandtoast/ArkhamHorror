module Arkham.Agenda.Cards.TheDunwichLegacy.ExtracurricularActivity.DeadOfNight (deadOfNight) where

import Arkham.Agenda.CardDefs.TheDunwichLegacy.ExtracurricularActivity qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.TheDunwichLegacy.ExtracurricularActivity qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv (getPhase)
import Arkham.Helpers.Modifiers
import Arkham.Location.CardDefs.TheDunwichLegacy.ExtracurricularActivity qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Phase

newtype DeadOfNight = DeadOfNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadOfNight :: AgendaCard DeadOfNight
deadOfNight = agenda (2, A) DeadOfNight Cards.deadOfNight (Static 3)

instance HasModifiersFor DeadOfNight where
  getModifiersFor (DeadOfNight a) = do
    phase <- getPhase
    modifySelectWhen a (phase == UpkeepPhase) Anyone [HandSize (-3)]

instance RunMessage DeadOfNight where
  runMessage msg a@(DeadOfNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      dormitories <- placeLocationIfNotInPlay Locations.dormitories

      selectOne (enemyIs Enemies.theExperiment) >>= \case
        Just eid -> moveToward eid dormitories
        Nothing -> createEnemyAtLocationMatching_ Enemies.theExperiment "Science Building"

      advanceAgendaDeck attrs
      pure a
    _ -> DeadOfNight <$> liftRunMessage msg attrs
