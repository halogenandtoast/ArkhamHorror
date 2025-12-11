module Arkham.Location.Cards.WealdOfEffigiesB (wealdOfEffigiesB) where

import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WithoutATrace.Helpers

newtype WealdOfEffigiesB = WealdOfEffigiesB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesB :: LocationCard WealdOfEffigiesB
wealdOfEffigiesB = symbolLabel $ locationWith WealdOfEffigiesB Cards.wealdOfEffigiesB 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesB where
  getAbilities (WealdOfEffigiesB a) =
    extendRevealed a []

instance RunMessage WealdOfEffigiesB where
  runMessage msg l@(WealdOfEffigiesB attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> pure ()
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> do
            assets <- selectWithField AssetCard $ assetControlledBy iid <> AssetCanLeavePlayByNormalMeans
            unless (null assets) do
              exposedInShadows iid attrs do
                chooseOneM iid $ for assets \(aid, card) -> targeting aid $ hollow iid card
      pure l
    _ -> WealdOfEffigiesB <$> liftRunMessage msg attrs
