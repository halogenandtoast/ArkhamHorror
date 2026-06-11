module Arkham.Act.Cards.WateryGraveGroupB (wateryGraveGroupB) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype WateryGraveGroupB = WateryGraveGroupB ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. By the time the
-- agenda advances, investigators at the same location must possess the
-- requisite number of clues.
wateryGraveGroupB :: ActCard WateryGraveGroupB
wateryGraveGroupB = act (1, A) WateryGraveGroupB Cards.wateryGraveGroupB Nothing

instance RunMessage WateryGraveGroupB where
  runMessage msg a@(WateryGraveGroupB attrs) = runQueueT $ scenarioI18n $ scope "wateryGrave" $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      n <- getPlayerCount
      locations <- select $ LocationWithInvestigator Anyone
      groups <- for locations \lid -> do
        investigators <- select $ InvestigatorAt $ LocationWithId lid
        total <- sum <$> traverse (field InvestigatorClues) investigators
        pure (investigators, total)
      case find ((>= 2 * n) . snd) (sortOn (negate . snd) groups) of
        Just (investigators, _) -> do
          flavor $ h "title" >> p "success"
          push $ SpendClues (2 * n) investigators
        Nothing -> do
          flavor $ h "title" >> p "failure"
          investigators <- allInvestigators
          for_ investigators \iid -> do
            clues <- field InvestigatorClues iid
            when (clues > 0) $ removeTokens (toSource attrs) iid #clue clues
          drowning <- selectOne $ InvestigatorAt $ locationIs Locations.chamberOfRain
          for_ drowning \iid -> push $ InvestigatorKilled (toSource attrs) iid
      push $ ScenarioSpecific "act2Setup" Null
      advanceActDeck attrs
      pure a
    _ -> WateryGraveGroupB <$> liftRunMessage msg attrs
