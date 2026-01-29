module Arkham.Investigator.Cards.DexterDrake2 (dexterDrake2) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Window
import Arkham.I18n
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DexterDrake2 = DexterDrake2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

dexterDrake2 :: InvestigatorCard DexterDrake2
dexterDrake2 =
  investigator DexterDrake2 Cards.dexterDrake2
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 2, agility = 3}

instance HasAbilities DexterDrake2 where
  getAbilities (DexterDrake2 a) =
    [ playerLimit PerRound
        $ selfAbility_ a 1
        $ triggered_
          ( PlayAsset #after You
              $ oneOf
                [ AssetWithDifferentTitleFromAtLeastOneCardInHand
                    You
                    (PlayableCard (UnpaidCost NoAction) #asset)
                    AnyAsset
                , AssetWithDifferentTitleFromAtLeastOneOtherAsset
                    PlayedAsset
                    (AssetNonStory <> AssetControlledBy You <> AssetCanLeavePlayByNormalMeans)
                ]
          )
    ]

instance HasChaosTokenValue DexterDrake2 where
  getChaosTokenValue iid ElderSign (DexterDrake2 attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage DexterDrake2 where
  runMessage msg i@(DexterDrake2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAsset -> aid) _ -> do
      assetName <- field AssetName aid
      assets <-
        select
          $ AssetNonStory
          <> not_ (AssetWithTitle assetName.title)
          <> assetControlledBy iid
          <> AssetCanLeavePlayByNormalMeans
      cards <- select $ inHandOf ForPlay iid <> basic (#asset <> not_ (CardWithTitle assetName.title))
      chooseOneM iid do
        targets assets $ returnToHand iid
        targets cards $ playCardPayingCost iid
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      assets <-
        selectWithField
          AssetOwner
          (AssetNonStory <> AssetAt (locationWithInvestigator iid) <> AssetCanLeavePlayByNormalMeans)
          <&> mapMaybe (\(a, mOwner) -> (a,) <$> mOwner)
      chooseOneM iid do
        for_ assets \(a, owner) -> targeting a $ returnToHand owner a
        withI18n skip_
      pure i
    _ -> DexterDrake2 <$> liftRunMessage msg attrs
