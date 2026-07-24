module Arkham.Act.Cards.SealedInGroupA (sealedInGroupA) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Query (allInvestigators, getPlayerCount)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype SealedInGroupA = SealedInGroupA ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedInGroupA :: ActCard SealedInGroupA
sealedInGroupA = act (1, A) SealedInGroupA Cards.sealedInGroupA Nothing

instance RunMessage SealedInGroupA where
  runMessage msg a@(SealedInGroupA attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- allInvestigators
      n <- getPlayerCount
      totalClues <- sum <$> fields InvestigatorClues investigators
      selectOne (assetIs Assets.keyOfMysteries <> AssetControlledBy Anyone) >>= \case
        Just key | totalClues >= 2 * n -> do
          spendCluesAsAGroup investigators (2 * n)
          removeFromGame key
          scenarioSpecific_ "act2Setup"
          advanceActDeck attrs
        _ -> push R1
      pure a
    _ -> SealedInGroupA <$> liftRunMessage msg attrs
