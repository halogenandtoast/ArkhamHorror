module Arkham.Agenda.Cards.ChildrenOfBlood.NewHorizons.QuietNight (quietNight) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype QuietNight = QuietNight AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quietNight :: AgendaCard QuietNight
quietNight = agenda (1, A) QuietNight Cards.quietNight (Static 6)

instance RunMessage QuietNight where
  runMessage msg a@(QuietNight attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      watchman <- getSetAsideCard Enemies.nightWatchman
      factoryFloors <- select $ NearestLocationToMost (LocationWithTitle "Factory Floor")
      leadChooseOrRunOneM $ targets factoryFloors (createEnemyAt_ watchman)
      advanceAgendaDeck attrs
      pure a
    _ -> QuietNight <$> liftRunMessage msg attrs
