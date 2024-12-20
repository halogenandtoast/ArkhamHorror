module Arkham.Agenda.Cards.EtherealTangleV1 (etherealTangleV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck

newtype EtherealTangleV1 = EtherealTangleV1 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV1 :: AgendaCard EtherealTangleV1
etherealTangleV1 = agenda (1, A) EtherealTangleV1 Cards.etherealTangleV1 (Static 15)

instance HasAbilities EtherealTangleV1 where
  getAbilities (EtherealTangleV1 a) =
    [ restricted a 1 (youExist $ not_ (at_ $ locationIs Locations.prisonOfMemories))
        $ FastAbility (HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage EtherealTangleV1 where
  runMessage msg a@(EtherealTangleV1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTo_ (attrs.ability 1) iid Locations.prisonOfMemories
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        addTekelili iid . take 1 =<< getScenarioDeck TekeliliDeck
        investigatorDefeated attrs iid
      pure a
    _ -> EtherealTangleV1 <$> liftRunMessage msg attrs
