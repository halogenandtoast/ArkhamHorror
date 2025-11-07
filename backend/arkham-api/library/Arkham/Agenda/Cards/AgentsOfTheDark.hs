module Arkham.Agenda.Cards.AgentsOfTheDark (agentsOfTheDark) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyEvaded)
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Window (evadedEnemy)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Projection
import Arkham.Scenarios.DealingsInTheDark.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token

newtype AgentsOfTheDark = AgentsOfTheDark AgendaAttrs
  deriving anyclass IsAgenda
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
          (enemyIsExact enemyCard)
          setActiveDuringSetup
          [AddKeyword $ Concealed concealedKind (Static 1)]

instance HasAbilities AgentsOfTheDark where
  getAbilities (AgentsOfTheDark a) = [mkAbility a 1 $ forced $ EnemyEvaded #after You #cultist]

instance RunMessage AgentsOfTheDark where
  runMessage msg a@(AgentsOfTheDark attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (evadedEnemy -> eid) _ -> do
      place eid InTheShadows
      resolveConcealed iid eid
      hasClues <- fieldMap EnemyTokens (hasToken Clue) eid
      when hasClues $ moveTokens (attrs.ability 1) eid iid Clue 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      theUnveiling <- selectJust $ storyIs Stories.theUnveiling
      let
        aboveEffect :: ReverseQueue m => InvestigatorId -> m ()
        aboveEffect iid = chooseOneM iid $ scenarioI18n do
          labeled' "agentsOfTheDark.cluesUnveiled" $ placeTokens attrs theUnveiling Clue 1
          labeled' "agentsOfTheDark.cultist" $ findAndDrawEncounterCard iid $ card_ $ #enemy <> #cultist
      investigators <- getInvestigators
      for_ investigators aboveEffect
      repeated (attrs.token Eclipse) $ leadChooseOrRunOneM $ targets investigators aboveEffect
      placeTokens attrs attrs Eclipse 1
      revertAgenda attrs.id
      pure a
    _ -> AgentsOfTheDark <$> liftRunMessage msg attrs
