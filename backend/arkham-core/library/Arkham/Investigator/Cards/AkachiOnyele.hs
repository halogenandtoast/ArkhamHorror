module Arkham.Investigator.Cards.AkachiOnyele where

import Arkham.Prelude

import Arkham.Asset.Types (Field (..))
import Arkham.Asset.Uses
import Arkham.Game.Helpers
import Arkham.Helpers.Use
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection

newtype AkachiOnyele = AkachiOnyele InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor AkachiOnyele where
  getModifiersFor (AssetTarget aid) (AkachiOnyele attrs) = do
    akachiAsset <- fieldP AssetController (== Just (toId attrs)) aid
    startingChargesCount <- fieldMapM AssetStartingUses (startingUseCountFor Charge) aid
    pure $ toModifiers attrs [AdditionalStartingUses 1 | akachiAsset, startingChargesCount > 0]
  getModifiersFor _ _ = pure []

akachiOnyele :: InvestigatorCard AkachiOnyele
akachiOnyele =
  investigator AkachiOnyele Cards.akachiOnyele
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 3, agility = 3}

instance HasChaosTokenValue AkachiOnyele where
  getChaosTokenValue iid ElderSign (AkachiOnyele attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AkachiOnyele where
  runMessage msg i@(AkachiOnyele attrs) = case msg of
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      assets <- filterByField AssetStartingUses (hasUsesFor Charge) =<< select (assetControlledBy iid)
      player <- getPlayer iid
      pushIfAny assets
        $ chooseOne player
        $ targetLabels assets (\asset -> only $ AddUses #elderSign asset Charge 1)
      pure i
    _ -> AkachiOnyele <$> runMessage msg attrs
