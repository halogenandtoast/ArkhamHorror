module Arkham.Homebrew.CircusExMortis.Agendas.SleepWhenYoureDead (sleepWhenYoureDead) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Matcher

newtype SleepWhenYoureDead = SleepWhenYoureDead AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepWhenYoureDead :: AgendaCard SleepWhenYoureDead
sleepWhenYoureDead =
  agenda (3, A) SleepWhenYoureDead Cards.sleepWhenYoureDead (Static 4)

instance HasAbilities SleepWhenYoureDead where
  getAbilities (SleepWhenYoureDead a) =
    [ restricted a 1 (if even a.doom then NoRestriction else Never)
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage SleepWhenYoureDead where
  runMessage msg a@(SleepWhenYoureDead attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      revealFuryToken (attrs.ability 1)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      sufferTraumaAndDefeat attrs
      pure a
    _ -> SleepWhenYoureDead <$> liftRunMessage msg attrs
