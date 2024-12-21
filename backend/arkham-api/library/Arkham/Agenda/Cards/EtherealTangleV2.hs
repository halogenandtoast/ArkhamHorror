module Arkham.Agenda.Cards.EtherealTangleV2 (etherealTangleV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Scenario.Deck

newtype EtherealTangleV2 = EtherealTangleV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV2 :: AgendaCard EtherealTangleV2
etherealTangleV2 = agenda (2, A) EtherealTangleV2 Cards.etherealTangleV2 (Static 13)

instance HasAbilities EtherealTangleV2 where
  getAbilities (EtherealTangleV2 a) =
    [ restricted a 1 (youExist $ not_ (at_ $ locationIs Locations.prisonOfMemories))
        $ FastAbility (HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage EtherealTangleV2 where
  runMessage msg a@(EtherealTangleV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTo_ (attrs.ability 1) iid Locations.prisonOfMemories
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        addTekelili iid . take 2 =<< getScenarioDeck TekeliliDeck
        investigatorDefeated attrs iid
      pure a
    _ -> EtherealTangleV2 <$> liftRunMessage msg attrs
