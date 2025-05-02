module Arkham.Act.Cards.TheParisianConspiracyV2 (theParisianConspiracyV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheParisianConspiracyV2 = TheParisianConspiracyV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theParisianConspiracyV2 :: ActCard TheParisianConspiracyV2
theParisianConspiracyV2 =
  act (1, A) TheParisianConspiracyV2 Cards.theParisianConspiracyV2 $ groupClueCost (PerPlayer 2)

instance HasAbilities TheParisianConspiracyV2 where
  getAbilities (TheParisianConspiracyV2 a) =
    extend1 a $ restricted a 1 (DoomCountIs $ atLeast 3) (Objective $ forced $ RoundEnds #when)

instance RunMessage TheParisianConspiracyV2 where
  runMessage msg a@(TheParisianConspiracyV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ advanceMode -> do
      theOrganist <-
        fromJustNote "The Organist was not set aside"
          . listToMaybe
          <$> getSetAsideCardsMatching (CardWithTitle "The Organist")
      case advanceMode of
        AdvancedWithClues -> do
          locations <- select $ FarthestLocationFromAll Anywhere
          lead <- getLead
          chooseOneM lead do
            questionLabeled "Where to spawn the organist"
            targets locations $ createEnemyAt_ theOrganist
        _ -> do
          location <- selectJust LeadInvestigatorLocation
          createEnemyAt_ theOrganist location
          eachInvestigator \iid -> assignHorror iid attrs 2
      advanceActDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    _ -> TheParisianConspiracyV2 <$> liftRunMessage msg attrs
