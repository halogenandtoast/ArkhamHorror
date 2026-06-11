module Arkham.Act.Cards.SealedInGroupA (sealedInGroupA) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers

newtype SealedInGroupA = SealedInGroupA ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Timed - Do not advance this act until you are instructed. By the time the
-- agenda advances, investigators must possess the Key of Mysteries and the
-- requisite number of clues.
sealedInGroupA :: ActCard SealedInGroupA
sealedInGroupA = act (1, A) SealedInGroupA Cards.sealedInGroupA Nothing

instance RunMessage SealedInGroupA where
  runMessage msg a@(SealedInGroupA attrs) = runQueueT $ scenarioI18n $ scope "sealedIn" $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- allInvestigators
      n <- getPlayerCount
      totalClues <- sum <$> traverse (field InvestigatorClues) investigators
      mKey <- selectOne $ assetIs Assets.keyOfMysteries <> AssetControlledBy Anyone
      case mKey of
        Just key | totalClues >= 2 * n -> do
          flavor $ h "title" >> p "success"
          push $ SpendClues (2 * n) investigators
          removeFromGame key
          push $ ScenarioSpecific "act2Setup" Null
          advanceActDeck attrs
        _ -> do
          flavor $ h "title" >> p "failure"
          push R1
      pure a
    _ -> SealedInGroupA <$> liftRunMessage msg attrs
