module Arkham.Agenda.Cards.JessiesRequest (jessiesRequest) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreationMethod (SpawnViaSpawnInstruction))
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Brotherhood))

newtype JessiesRequest = JessiesRequest AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jessiesRequest :: AgendaCard JessiesRequest
jessiesRequest = agenda (1, A) JessiesRequest Cards.jessiesRequest (Static 7)

instance HasAbilities JessiesRequest where
  getAbilities (JessiesRequest a) =
    [ restricted a 1 (exists $ UnderScenarioReferenceMatch AnyCard)
        $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) Anywhere)
    ]

instance RunMessage JessiesRequest where
  runMessage msg a@(JessiesRequest attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cards <- scenarioField ScenarioCardsUnderScenarioReference
      for_ (nonEmpty cards) \cards' -> do
        card <- sample cards'
        createEnemy_ card SpawnViaSpawnInstruction
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      enemies <- select $ EnemyWithTrait Brotherhood
      for_ enemies \enemy -> do
        card <- field EnemyCard enemy
        push $ RemoveEnemy enemy
        push $ PlaceUnderneath ScenarioTarget [card]
      revenants <- getSetAsideCardsMatching $ cardIs Enemies.abyssalRevenant
      shuffleCardsIntoDeck Deck.EncounterDeck revenants
      shuffleEncounterDiscardBackIn
      addStrengthOfTheAbyss 1
      step <- getCurrentActStep
      when (step == 1) do
        lead <- getLead
        actId <- selectJust AnyAct
        push $ AdvanceAct actId (toSource lead) #other
      advanceAgendaDeck attrs
      pure a
    _ -> JessiesRequest <$> liftRunMessage msg attrs
