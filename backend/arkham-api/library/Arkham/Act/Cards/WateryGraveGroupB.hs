module Arkham.Act.Cards.WateryGraveGroupB (wateryGraveGroupB) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection

newtype WateryGraveGroupB = WateryGraveGroupB ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wateryGraveGroupB :: ActCard WateryGraveGroupB
wateryGraveGroupB = act (1, A) WateryGraveGroupB Cards.wateryGraveGroupB Nothing

instance RunMessage WateryGraveGroupB where
  runMessage msg a@(WateryGraveGroupB attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      n <- getPlayerCount
      locations <- select $ LocationWithInvestigator Anyone
      groups <- for locations \lid -> do
        investigators <- select $ InvestigatorAt $ LocationWithId lid
        total <- sum <$> traverse (field InvestigatorClues) investigators
        pure (investigators, total)
      case find ((>= 2 * n) . snd) (sortOn (negate . snd) groups) of
        Just (investigators, _) -> spendCluesAsAGroup investigators (2 * n)
        Nothing -> do
          eachInvestigator \iid -> do
            clues <- field InvestigatorClues iid
            when (clues > 0) $ removeTokens (toSource attrs) iid #clue clues
          drowning <- selectOne $ InvestigatorAt $ locationIs Locations.chamberOfRain
          for_ drowning $ push . InvestigatorKilled (toSource attrs)
      scenarioSpecific_ "act2Setup"
      advanceActDeck attrs
      pure a
    _ -> WateryGraveGroupB <$> liftRunMessage msg attrs
