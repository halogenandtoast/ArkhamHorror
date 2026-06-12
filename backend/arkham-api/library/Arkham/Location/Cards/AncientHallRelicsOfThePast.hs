module Arkham.Location.Cards.AncientHallRelicsOfThePast (ancientHallRelicsOfThePast) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.RelicsOfThePast.Helpers

newtype AncientHallRelicsOfThePast = AncientHallRelicsOfThePast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHallRelicsOfThePast :: LocationCard AncientHallRelicsOfThePast
ancientHallRelicsOfThePast =
  symbolLabel
    $ location AncientHallRelicsOfThePast Cards.ancientHallRelicsOfThePast 3 (PerPlayer 2)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities AncientHallRelicsOfThePast where
  getAbilities (AncientHallRelicsOfThePast a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ PlacedCounterOnLocation #after (be a) AnySource #doom (atLeast 1)

instance RunMessage AncientHallRelicsOfThePast where
  runMessage msg l@(AncientHallRelicsOfThePast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      let shuffleTopCard =
            getEncounterDeck >>= \case
              Deck [] -> pure ()
              Deck (x : _) -> shuffleCardsIntoDeck ExplorationDeck [x]
      investigators <- select $ investigatorAt (toId attrs) <> InvestigatorCanSpendResources (Static 3)
      if null investigators
        then shuffleTopCard
        else leadChooseOneM $ scenarioI18n do
          questionLabeled' "ancientHall.question"
          labeled' "ancientHall.doNotCancel" shuffleTopCard
          targets investigators $ spendResourcesOf 3
      pure l
    _ -> AncientHallRelicsOfThePast <$> liftRunMessage msg attrs
