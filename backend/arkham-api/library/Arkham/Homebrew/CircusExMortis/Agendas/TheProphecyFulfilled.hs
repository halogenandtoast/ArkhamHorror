module Arkham.Homebrew.CircusExMortis.Agendas.TheProphecyFulfilled (theProphecyFulfilled) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Agendas.TheProphecy
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards

newtype TheProphecyFulfilled = TheProphecyFulfilled AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theProphecyFulfilled :: AgendaCard TheProphecyFulfilled
theProphecyFulfilled = agenda (2, A) TheProphecyFulfilled Cards.theProphecyFulfilled (Static 3)

instance HasAbilities TheProphecyFulfilled where
  getAbilities (TheProphecyFulfilled a) = prophecyAbilities a

instance RunMessage TheProphecyFulfilled where
  runMessage msg a@(TheProphecyFulfilled attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeShubNiggurathsDamage 3 (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      removeDoomFromEveryOtherCard attrs msg
      pure a
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 2) -> do
      shubNiggurathAttacks attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceAgenda attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      don't $ ForTarget (toTarget attrs) AdvanceAgendaIfThresholdSatisfied
      push R2
      pure a
    -- Back: "(→R1)"
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    _ -> TheProphecyFulfilled <$> liftRunMessage msg attrs
