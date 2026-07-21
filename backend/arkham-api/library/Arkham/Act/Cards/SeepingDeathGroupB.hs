module Arkham.Act.Cards.SeepingDeathGroupB (seepingDeathGroupB) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Name (Labeled (..))
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey

newtype SeepingDeathGroupB = SeepingDeathGroupB ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seepingDeathGroupB :: ActCard SeepingDeathGroupB
seepingDeathGroupB = act (2, A) SeepingDeathGroupB Cards.seepingDeathGroupB Nothing

instance RunMessage SeepingDeathGroupB where
  runMessage msg a@(SeepingDeathGroupB attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      remembered' <- scenarioFieldMap ScenarioRemembered toList
      let
        injected iid = flip any remembered' \case
          BeenInjected (Labeled _ iid') -> iid == iid'
          _ -> False
        mValveTurner =
          listToMaybe $ flip mapMaybe remembered' \case
            TurnedTheValve (Labeled _ iid) -> Just iid
            _ -> Nothing
      case mValveTurner of
        Just iid ->
          unless (injected iid) do
            push $ InvestigatorKilled (toSource attrs) iid
        Nothing -> do
          poison <- selectJust $ locationIs Locations.chamberOfPoison
          investigators <- allInvestigators
          for_ investigators \iid -> do
            moveTo attrs iid poison
            unless (injected iid) $ push $ InvestigatorKilled (toSource attrs) iid
      scenarioSpecific_ "act3Setup"
      advanceActDeck attrs
      pure a
    _ -> SeepingDeathGroupB <$> liftRunMessage msg attrs
