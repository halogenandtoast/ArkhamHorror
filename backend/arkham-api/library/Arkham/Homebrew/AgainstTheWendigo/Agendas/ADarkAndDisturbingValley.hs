module Arkham.Homebrew.AgainstTheWendigo.Agendas.ADarkAndDisturbingValley (
  aDarkAndDisturbingValley,
) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgainstTheWendigo.Sets qualified as Set
import Arkham.Matcher

newtype ADarkAndDisturbingValley = ADarkAndDisturbingValley AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aDarkAndDisturbingValley :: AgendaCard ADarkAndDisturbingValley
aDarkAndDisturbingValley =
  agenda (1, A) ADarkAndDisturbingValley Cards.aDarkAndDisturbingValley (Static 4)

instance RunMessage ADarkAndDisturbingValley where
  runMessage msg a@(ADarkAndDisturbingValley attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- Setup set the whole set aside, so The Wendigo just stays there.
      shuffleSetAsideIntoEncounterDeck
        $ CardFromEncounterSet Set.WendigosMyth
        <> not_ (cardIs Enemies.theWendigo)
      shuffleEncounterDiscardBackIn
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed 4)
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directHorror iid attrs 1
      pure a
    _ -> ADarkAndDisturbingValley <$> liftRunMessage msg attrs
