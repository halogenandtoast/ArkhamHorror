module Arkham.Enemy.Cards.CovenInitiate (covenInitiate) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Hex))

newtype CovenInitiate = CovenInitiate EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

covenInitiate :: EnemyCard CovenInitiate
covenInitiate = enemy CovenInitiate Cards.covenInitiate (2, Static 2, 2) (0, 1)

instance RunMessage CovenInitiate where
  runMessage msg e@(CovenInitiate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discardTopOfEncounterDeckAndHandle iid attrs 2 attrs
      pure e
    DiscardedTopOfEncounterDeck iid _ _ (isTarget attrs -> True) -> do
      deckEmpty <- scenarioFieldMap ScenarioEncounterDeck null
      when deckEmpty $ do
        mHexCard <- findTopOfDiscard (CardWithTrait Hex)
        for_ mHexCard \hex -> drawCardFrom iid hex Deck.EncounterDiscard
      pure e
    _ -> CovenInitiate <$> liftRunMessage msg attrs
