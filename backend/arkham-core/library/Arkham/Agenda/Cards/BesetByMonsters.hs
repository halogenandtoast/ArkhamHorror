module Arkham.Agenda.Cards.BesetByMonsters (BesetByMonsters (..), besetByMonsters) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner hiding (advanceAgendaDeck, chooseOrRunOne)
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype BesetByMonsters = BesetByMonsters AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

besetByMonsters :: AgendaCard BesetByMonsters
besetByMonsters = agenda (2, A) BesetByMonsters Cards.besetByMonsters (Static 4)

instance RunMessage BesetByMonsters where
  runMessage msg a@(BesetByMonsters attrs) = runQueueT $ case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      -- Place 1 damage on the scenario reference card. Then:
      -- If there are 5 or more damage tokens on the scenario reference card, proceed to (→R2).
      eachInvestigator $ \iid -> do
        resources <- field InvestigatorResources iid
        hand <- field InvestigatorHand iid

        when (resources > 0 || notNull hand) do
          chooseOrRunOne iid
            $ [Label "Discard 1 card at random" [toMessage $ randomDiscard iid attrs] | notNull hand]
            <> [Label "Lost 2 resources" [LoseResources iid (toSource attrs) 2] | resources > 0]
      shuffleEncounterDiscardBackIn
      push $ ScenarioCountIncrementBy Distortion 1
      push $ DoStep 1 msg
      pure a
    DoStep 1 (AdvanceAgenda aid) | aid == toId attrs && onSide B attrs -> do
      n <- scenarioCount Distortion
      if n >= 5
        then push R2
        else revertAgenda attrs
      pure a
    _ -> BesetByMonsters <$> liftRunMessage msg attrs
