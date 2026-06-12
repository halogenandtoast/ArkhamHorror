module Arkham.Scenarios.ReadOrDie.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing
import Arkham.Trait (Trait (Tome))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "readOrDie" a

daisyWalker :: InvestigatorMatcher
daisyWalker = InvestigatorWithTitle "Daisy Walker"

nonWeaknessTomeAsset :: CardMatcher
nonWeaknessTomeAsset = CardWithType AssetType <> CardWithTrait Tome <> NonWeakness

-- Checks the investigator's deck, hand, discard, and in-play assets for a
-- copy of the given card so resolution swaps are only offered when legal.
hasInPool :: (HasGame m, Tracing m) => InvestigatorId -> CardDef -> m Bool
hasInPool iid def = do
  deck <- fieldMap InvestigatorDeck (map toCard . unDeck) iid
  hand <- field InvestigatorHand iid
  discard <- fieldMap InvestigatorDiscard (map toCard) iid
  inPlay <- selectAny $ assetIs def <> AssetOwnedBy (InvestigatorWithId iid)
  pure $ inPlay || any ((== def) . toCardDef) (deck <> hand <> discard)
