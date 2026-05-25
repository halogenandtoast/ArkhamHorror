module Arkham.Act.Cards.ABotanicalSurvey (aBotanicalSurvey) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Constants
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheThingInTheDepths.Helpers

newtype ABotanicalSurvey = ABotanicalSurvey ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBotanicalSurvey :: ActCard ABotanicalSurvey
aBotanicalSurvey = act (1, A) ABotanicalSurvey Cards.aBotanicalSurvey Nothing

instance HasAbilities ABotanicalSurvey where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      ActAdvancement
      (exists $ InvestigatorAt $ LocationInPosition northShorePos)
      $ Objective
      $ triggered (RoundEnds #when)
      $ GroupClueCost (PerPlayer 3) (LocationInPosition northShorePos)

instance RunMessage ABotanicalSurvey where
  runMessage msg a@(ABotanicalSurvey attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      northShore <- selectJust $ LocationInPosition northShorePos
      createEnemyAt_ Enemies.chelydranHybrid northShore

      adjacentLocs <- select $ connectedTo (LocationWithId northShore)
      lead <- getLead
      tendrils <- getSetAsideCardsMatching (cardIs Enemies.graspingTendril)
      for_ (nonEmpty tendrils) \(toSpawn :| toShuffle) -> do
        chooseTargetM lead adjacentLocs $ createEnemyAt_ toSpawn
        push $ ShuffleCardsIntoDeck Deck.EncounterDeck toShuffle
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> ABotanicalSurvey <$> liftRunMessage msg attrs
