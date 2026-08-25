module Arkham.Homebrew.CircusExMortis.Agendas.TheCircusSleeps (theCircusSleeps) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher

newtype TheCircusSleeps = TheCircusSleeps AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCircusSleeps :: AgendaCard TheCircusSleeps
theCircusSleeps =
  agenda (1, A) TheCircusSleeps Cards.theCircusSleeps (Static 6)

instance HasAbilities TheCircusSleeps where
  getAbilities (TheCircusSleeps a) =
    [ restricted a 1 (if even a.doom then NoRestriction else Never)
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage TheCircusSleeps where
  runMessage msg a@(TheCircusSleeps attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      revealFuryToken (attrs.ability 1)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      addFuryToken MoonToken
      advanceAgendaDeck attrs
      pure a
    _ -> TheCircusSleeps <$> liftRunMessage msg attrs
