module Arkham.Agenda.Cards.TheBrotherhoodBidesTheirTime (theBrotherhoodBidesTheirTime) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Creation (EnemyCreationMethod (SpawnEngagedWith))
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Helpers.Scenario (scenarioField)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Types (Field (..))

newtype TheBrotherhoodBidesTheirTime = TheBrotherhoodBidesTheirTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBrotherhoodBidesTheirTime :: AgendaCard TheBrotherhoodBidesTheirTime
theBrotherhoodBidesTheirTime =
  agenda (1, A) TheBrotherhoodBidesTheirTime Cards.theBrotherhoodBidesTheirTime (Static 6)

instance RunMessage TheBrotherhoodBidesTheirTime where
  runMessage msg a@(TheBrotherhoodBidesTheirTime attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      cards <- shuffle =<< scenarioField ScenarioCardsUnderScenarioReference
      investigators <- getInvestigators
      let (engaged, rest) = splitAt (length investigators) cards
      for_ (zip investigators engaged) \(iid, card) ->
        createEnemy_ card (SpawnEngagedWith iid)
      for_ rest \card -> doStep 1 (ForTarget (CardIdTarget card.id) msg)
      shuffleEncounterDiscardBackIn
      addStrengthOfTheAbyss 1
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (ForTarget (CardIdTarget cid) (AdvanceAgenda (isSide B attrs -> True))) -> do
      card <- getCard cid
      expeditionCamp <- selectJust $ LocationWithTitle "Expedition Camp"
      locations <- select $ EmptyLocation <> FarthestLocationFromLocation expeditionCamp Anywhere
      lead <- getLead
      chooseOrRunTargetM lead locations \loc -> createEnemyAt_ card loc
      pure a
    _ -> TheBrotherhoodBidesTheirTime <$> liftRunMessage msg attrs
