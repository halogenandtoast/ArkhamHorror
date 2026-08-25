module Arkham.Homebrew.CircusExMortis.Agendas.TreadingOnEggshells (treadingOnEggshells) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Matcher

newtype TreadingOnEggshells = TreadingOnEggshells AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treadingOnEggshells :: AgendaCard TreadingOnEggshells
treadingOnEggshells =
  agenda (2, A) TreadingOnEggshells Cards.treadingOnEggshells (Static 6)

instance HasAbilities TreadingOnEggshells where
  getAbilities (TreadingOnEggshells a) =
    [ restricted a 1 (if even a.doom then NoRestriction else Never)
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage TreadingOnEggshells where
  runMessage msg a@(TreadingOnEggshells attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      revealFuryToken (attrs.ability 1)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      addFuryToken MoonToken
      advanceAgendaDeck attrs
      pure a
    _ -> TreadingOnEggshells <$> liftRunMessage msg attrs
