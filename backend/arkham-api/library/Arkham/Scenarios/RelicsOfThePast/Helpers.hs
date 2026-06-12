module Arkham.Scenarios.RelicsOfThePast.Helpers where

import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Tracing
import Arkham.Trait (Trait (Ancient))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "relicsOfThePast" a

scenarioSupplies :: [Supply]
scenarioSupplies = [Chalk, Compass, Journal, Satchel, Torches]

supplyKey :: Supply -> Scope
supplyKey = \case
  Chalk -> "supplies.chalk"
  Compass -> "supplies.compass"
  Journal -> "supplies.journal"
  Satchel -> "supplies.satchel"
  Torches -> "supplies.torches"
  _ -> "supplies.chalk"

getPickedSupplies :: (HasGame m, Tracing m) => m (Map InvestigatorId [Supply])
getPickedSupplies = scenarioFieldMap ScenarioMeta (toResultDefault mempty)

shuffleAncientAssetsIntoExplorationDeck :: ReverseQueue m => InvestigatorId -> m ()
shuffleAncientAssetsIntoExplorationDeck iid = do
  assets <- select $ AssetWithTrait Ancient <> AssetControlledBy (InvestigatorWithId iid)
  for_ assets $ shuffleIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck)
