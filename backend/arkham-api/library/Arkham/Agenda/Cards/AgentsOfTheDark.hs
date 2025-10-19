module Arkham.Agenda.Cards.AgentsOfTheDark (agentsOfTheDark) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Modifier (setActiveDuringSetup)

newtype AgentsOfTheDark = AgentsOfTheDark AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentsOfTheDark :: AgendaCard AgentsOfTheDark
agentsOfTheDark = agenda (1, A) AgentsOfTheDark Cards.agentsOfTheDark (Static 4)

instance HasModifiersFor AgentsOfTheDark where
  getModifiersFor (AgentsOfTheDark a) = do
    for_
      [ (Enemies.acolyte, AcolyteAny)
      , (Enemies.wizardOfTheOrder, WizardOfTheOrder)
      , (Enemies.sinisterAspirantA, SinisterAspirantA)
      , (Enemies.sinisterAspirantB, SinisterAspirantB)
      , (Enemies.sinisterAspirantC, SinisterAspirantC)
      ]
      \(enemyCard, concealedKind) ->
        modifySelectWith
          a
          (enemyIs enemyCard)
          setActiveDuringSetup
          [AddKeyword $ Concealed concealedKind (Static 1)]

instance RunMessage AgentsOfTheDark where
  runMessage msg a@(AgentsOfTheDark attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> AgentsOfTheDark <$> liftRunMessage msg attrs
