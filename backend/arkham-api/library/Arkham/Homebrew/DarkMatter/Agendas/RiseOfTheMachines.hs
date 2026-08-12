module Arkham.Homebrew.DarkMatter.Agendas.RiseOfTheMachines (riseOfTheMachines) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction_)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Resolution

newtype RiseOfTheMachines = RiseOfTheMachines AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riseOfTheMachines :: AgendaCard RiseOfTheMachines
riseOfTheMachines = agenda (3, A) RiseOfTheMachines Cards.riseOfTheMachines (Static 9)

instance HasAbilities RiseOfTheMachines where
  getAbilities (RiseOfTheMachines a) =
    [restricted a 1 (exists $ YourLocation <> LocationWithoutClues) scanAction_]

instance RunMessage RiseOfTheMachines where
  runMessage msg a@(RiseOfTheMachines attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      push $ ScenarioResolution NoResolution
      pure a
    _ -> RiseOfTheMachines <$> liftRunMessage msg attrs
