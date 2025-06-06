module Arkham.Location.Cards.EntrywayRearrangedByTime (entrywayRearrangedByTime) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype EntrywayRearrangedByTime = EntrywayRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entrywayRearrangedByTime :: LocationCard EntrywayRearrangedByTime
entrywayRearrangedByTime =
  location EntrywayRearrangedByTime Cards.entrywayRearrangedByTime 2 (PerPlayer 1)
    & setLabel "entryway"
    & setConnectsTo (singleton LeftOf)

instance HasAbilities EntrywayRearrangedByTime where
  getAbilities (EntrywayRearrangedByTime a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "entryway.resign" $ locationResignAction a
      , restricted a 1 (HasSupply Torches) actionAbility
      ]

instance RunMessage EntrywayRearrangedByTime where
  runMessage msg l@(EntrywayRearrangedByTime attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      explorationDeck <- getExplorationDeck
      let (viewing, rest) = splitAt 2 explorationDeck
      let (discardable, other) = partition (`cardMatch` mapOneOf CardWithType [TreacheryType, EnemyType]) viewing
      focusCards viewing do
        setScenarioDeck ExplorationDeck $ other <> rest
        chooseOneM iid $ targets (onlyEncounterCards discardable) (addToEncounterDiscard . only)
      for_ other $ putCardOnBottomOfDeck iid ExplorationDeck
      shuffleDeck ExplorationDeck
      pure l
    _ -> EntrywayRearrangedByTime <$> liftRunMessage msg attrs
