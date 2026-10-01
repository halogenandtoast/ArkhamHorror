module Arkham.Homebrew.CircusExMortis.Agendas.TheProphecyUnfulfilled (theProphecyUnfulfilled) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Agendas.TheProphecy
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards

newtype TheProphecyUnfulfilled = TheProphecyUnfulfilled AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theProphecyUnfulfilled :: AgendaCard TheProphecyUnfulfilled
theProphecyUnfulfilled = agenda (2, A) TheProphecyUnfulfilled Cards.theProphecyUnfulfilled (Static 3)

instance HasAbilities TheProphecyUnfulfilled where
  getAbilities (TheProphecyUnfulfilled a) = prophecyAbilities a

instance RunMessage TheProphecyUnfulfilled where
  runMessage msg a@(TheProphecyUnfulfilled attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeShubNiggurathsDamage 5 (attrs.ability 1)
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
    _ -> TheProphecyUnfulfilled <$> liftRunMessage msg attrs
