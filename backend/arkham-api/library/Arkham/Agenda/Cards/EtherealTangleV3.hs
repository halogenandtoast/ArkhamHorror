module Arkham.Agenda.Cards.EtherealTangleV3 (etherealTangleV3) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype EtherealTangleV3 = EtherealTangleV3 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealTangleV3 :: AgendaCard EtherealTangleV3
etherealTangleV3 = agenda (3, A) EtherealTangleV3 Cards.etherealTangleV3 (Static 11)

instance HasAbilities EtherealTangleV3 where
  getAbilities (EtherealTangleV3 a) =
    [ restricted a 1 (youExist $ not_ (at_ $ locationIs Locations.prisonOfMemories))
        $ FastAbility (HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage EtherealTangleV3 where
  runMessage msg a@(EtherealTangleV3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveTo_ (attrs.ability 1) iid Locations.prisonOfMemories
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> EtherealTangleV3 <$> liftRunMessage msg attrs
