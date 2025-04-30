module Arkham.Agenda.Cards.DeadOfNight (deadOfNight) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype DeadOfNight = DeadOfNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadOfNight :: AgendaCard DeadOfNight
deadOfNight = agenda (2, A) DeadOfNight Cards.deadOfNight (Static 3)

instance HasModifiersFor DeadOfNight where
  getModifiersFor (DeadOfNight a) = modifySelect a Anyone [HandSize (-3)]

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
