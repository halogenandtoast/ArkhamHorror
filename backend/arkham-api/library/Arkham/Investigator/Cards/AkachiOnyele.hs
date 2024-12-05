module Arkham.Investigator.Cards.AkachiOnyele where

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Game.Helpers
import Arkham.Helpers.Use
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype AkachiOnyele = AkachiOnyele InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor AkachiOnyele where
  getModifiersFor (AkachiOnyele a) =
    modifySelect
      a
      (AssetControlledBy (InvestigatorWithId a.id) <> AssetWithUseType Charge)
      [AdditionalStartingUses 1]

akachiOnyele :: InvestigatorCard AkachiOnyele
akachiOnyele =
  investigator AkachiOnyele Cards.akachiOnyele
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 3, agility = 3}

instance HasChaosTokenValue AkachiOnyele where
  getChaosTokenValue iid ElderSign (AkachiOnyele attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AkachiOnyele where
  runMessage msg i@(AkachiOnyele attrs) = runQueueT $ case msg of
    ElderSignEffect iid | attrs `is` iid -> do
      assets <- filterByField AssetStartingUses (hasUsesFor Charge) =<< select (assetControlledBy iid)
      chooseTargetM iid assets \asset -> push $ AddUses #elderSign asset Charge 1
      pure i
    _ -> AkachiOnyele <$> liftRunMessage msg attrs
