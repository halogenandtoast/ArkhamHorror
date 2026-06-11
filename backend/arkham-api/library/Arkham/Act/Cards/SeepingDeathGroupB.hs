module Arkham.Act.Cards.SeepingDeathGroupB (seepingDeathGroupB) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Name (Labeled (..))
import Arkham.Scenario.Types (Field (..))
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype SeepingDeathGroupB = SeepingDeathGroupB ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. Before the
-- agenda advances, one investigator must activate the [action] ability on
-- the Chamber of Poison.
seepingDeathGroupB :: ActCard SeepingDeathGroupB
seepingDeathGroupB = act (2, A) SeepingDeathGroupB Cards.seepingDeathGroupB Nothing

instance RunMessage SeepingDeathGroupB where
  runMessage msg a@(SeepingDeathGroupB attrs) = runQueueT $ scenarioI18n $ scope "seepingDeath" $ case msg of
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
          if injected iid
            then flavor $ h "title" >> p "injected"
            else do
              flavor $ h "title" >> p "notInjected"
              push $ InvestigatorKilled (toSource attrs) iid
        Nothing -> do
          flavor $ h "title" >> p "noValve"
          poison <- selectJust $ locationIs Locations.chamberOfPoison
          investigators <- allInvestigators
          for_ investigators \iid -> do
            moveTo attrs iid poison
            unless (injected iid) $ push $ InvestigatorKilled (toSource attrs) iid
      push $ ScenarioSpecific "act3Setup" Null
      advanceActDeck attrs
      pure a
    _ -> SeepingDeathGroupB <$> liftRunMessage msg attrs
