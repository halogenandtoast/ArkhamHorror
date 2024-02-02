module Arkham.Enemy.Cards.CovenInitiate (
  covenInitiate,
  CovenInitiate (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Hex))

newtype CovenInitiate = CovenInitiate EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

covenInitiate :: EnemyCard CovenInitiate
covenInitiate = enemy CovenInitiate Cards.covenInitiate (2, Static 2, 2) (0, 1)

instance RunMessage CovenInitiate where
  runMessage msg e@(CovenInitiate attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ DiscardTopOfEncounterDeck iid 2 (toSource attrs) (Just $ toTarget attrs)
      pure e
    DiscardedTopOfEncounterDeck iid _ _ (isTarget attrs -> True) -> do
      deckEmpty <- scenarioFieldMap ScenarioEncounterDeck null
      when deckEmpty $ do
        mHexCard <- findTopOfDiscard (CardWithTrait Hex)
        for_
          mHexCard
          (push . InvestigatorDrewEncounterCard iid)
      pure e
    _ -> CovenInitiate <$> runMessage msg attrs
