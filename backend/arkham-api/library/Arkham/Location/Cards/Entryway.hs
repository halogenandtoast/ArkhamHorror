module Arkham.Location.Cards.Entryway (entryway) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype Entryway = Entryway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryway :: LocationCard Entryway
entryway = location Entryway Cards.entryway 2 (PerPlayer 1) & setConnectsTo (singleton LeftOf)

instance HasAbilities Entryway where
  getAbilities (Entryway a) =
    extendRevealed
      a
      [ scenarioI18n $ withI18nTooltip "entryway.resign" $ locationResignAction a
      , restricted a 1 (Here <> HasSupply Torches) actionAbility
      ]

instance RunMessage Entryway where
  runMessage msg l@(Entryway attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      explorationDeck <- getExplorationDeck
      let (viewing, rest) = splitAt 2 explorationDeck
      let (treacheries, other) = partition (`cardMatch` CardWithType TreacheryType) viewing
      focusCards viewing do
        setScenarioDeck ExplorationDeck $ other <> rest
        chooseOneAtATimeM iid do
          targets (onlyEncounterCards treacheries) (addToEncounterDiscard . only)
      for_ other $ putCardOnBottomOfDeck iid ExplorationDeck
      shuffleDeck ExplorationDeck
      pure l
    _ -> Entryway <$> liftRunMessage msg attrs
