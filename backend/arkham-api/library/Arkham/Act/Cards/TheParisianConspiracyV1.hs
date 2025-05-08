module Arkham.Act.Cards.TheParisianConspiracyV1 (theParisianConspiracyV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card.CardCode
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario (getIsReturnTo)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.APhantomOfTruth.Helpers

newtype TheParisianConspiracyV1 = TheParisianConspiracyV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV1 :: ActCard TheParisianConspiracyV1
theParisianConspiracyV1 =
  act (1, A) TheParisianConspiracyV1 Cards.theParisianConspiracyV1 $ groupClueCost (PerPlayer 2)

instance HasAbilities TheParisianConspiracyV1 where
  getAbilities (TheParisianConspiracyV1 a) =
    extend1 a $ restricted a 1 (DoomCountIs $ atLeast 3) (Objective $ forced $ RoundEnds #when)

instance RunMessage TheParisianConspiracyV1 where
  runMessage msg a@(TheParisianConspiracyV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ advanceMode -> do
      theOrganist <-
        fromJustNote "The Organist was not set aside"
          . listToMaybe
          <$> getSetAsideCardsMatching "The Organist"
      case advanceMode of
        AdvancedWithClues -> do
          location <- selectJust LeadInvestigatorLocation
          createEnemyAt_ theOrganist location
        _ -> do
          eachInvestigator \iid -> assignHorror iid attrs 2
          locations <- select $ FarthestLocationFromAll Anywhere
          leadChooseOneM do
            scenarioI18n $ questionLabeled' "theParisianConspiracy.spawn"
            targets locations (createEnemyAt_ theOrganist)

      whenM getIsReturnTo do
        lead <- getLead
        leadChooseOneM do
          abilityLabeled
            lead
            (mkAbility (SourceableWithCardCode (CardCode "52040") ScenarioSource) 1 $ forced AnyWindow)
            nothing
      advanceActDeck attrs
      pure a
    _ -> TheParisianConspiracyV1 <$> liftRunMessage msg attrs
