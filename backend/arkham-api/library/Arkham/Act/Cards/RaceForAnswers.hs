module Arkham.Act.Cards.RaceForAnswers (raceForAnswers) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card.CardCode
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype RaceForAnswers = RaceForAnswers ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

raceForAnswers :: ActCard RaceForAnswers
raceForAnswers = act (1, A) RaceForAnswers Cards.raceForAnswers (groupClueCost (PerPlayer 2))

instance RunMessage RaceForAnswers where
  runMessage msg a@(RaceForAnswers attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      playerCount <- getPlayerCount
      selectEach (RevealedLocation <> "Historical Society") \location ->
        push $ PlaceCluesUpToClueValue location (toSource attrs) playerCount
      advanceActDeck attrs
      whenM getIsReturnTo do
        ok <- selectAny $ EmptyLocation <> "Historical Society"
        when ok do
          lead <- getLead
          leadChooseOneM do
            abilityLabeled
              lead
              (mkAbility (SourceableWithCardCode (CardCode "52028") ScenarioSource) 1 $ forced AnyWindow)
              nothing
      pure a
    _ -> RaceForAnswers <$> liftRunMessage msg attrs
