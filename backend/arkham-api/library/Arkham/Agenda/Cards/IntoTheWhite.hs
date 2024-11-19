module Arkham.Agenda.Cards.IntoTheWhite (IntoTheWhite (..), intoTheWhite) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Window (entering)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.IceAndDeath.Helpers

newtype IntoTheWhite = IntoTheWhite AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheWhite :: AgendaCard IntoTheWhite
intoTheWhite = agenda (2, A) IntoTheWhite Cards.intoTheWhite (Static 7)

instance HasAbilities IntoTheWhite where
  getAbilities (IntoTheWhite a) = [placeSetAsideConnectedAbility a 1]

instance RunMessage IntoTheWhite where
  runMessage msg a@(IntoTheWhite attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      locations <- selectForEach RevealedLocation \lid -> do
        (lid,) . fromMaybe (-1) <$> shelterValue lid

      lead <- getLead

      chooseOrRunOneM lead do
        targets (maxes locations) $ createSetAsideEnemy_ Enemies.terrorOfTheStarsBringerOfIceAndDeath

      advanceAgendaDeck attrs
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (entering -> lid) _ -> do
      placeSetAsideConnectedLocations lid
      pure a
    _ -> IntoTheWhite <$> liftRunMessage msg attrs
