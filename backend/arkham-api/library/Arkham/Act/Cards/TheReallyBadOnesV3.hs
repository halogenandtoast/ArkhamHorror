module Arkham.Act.Cards.TheReallyBadOnesV3 (theReallyBadOnesV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait

newtype TheReallyBadOnesV3 = TheReallyBadOnesV3 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theReallyBadOnesV3 :: ActCard TheReallyBadOnesV3
theReallyBadOnesV3 = act (2, A) TheReallyBadOnesV3 Cards.theReallyBadOnesV3 Nothing

instance HasModifiersFor TheReallyBadOnesV3 where
  getModifiersFor (TheReallyBadOnesV3 attrs) = do
    modifySelect attrs UnrevealedLocation [TraitRestrictedModifier ArkhamAsylum Blank]

instance RunMessage TheReallyBadOnesV3 where
  runMessage msg a@(TheReallyBadOnesV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      createEnemyAtLocationMatching_
        Enemies.hostOfInsanity
        (locationIs Locations.patientConfinementDanielsCell)
      enemiesUnderAct <- filter ((== EnemyType) . toCardType) <$> scenarioField ScenarioCardsUnderActDeck
      shuffleCardsIntoDeck Deck.EncounterDeck enemiesUnderAct
      shuffleEncounterDiscardBackIn
      advanceActDeck attrs
      pure a
    _ -> TheReallyBadOnesV3 <$> liftRunMessage msg attrs
