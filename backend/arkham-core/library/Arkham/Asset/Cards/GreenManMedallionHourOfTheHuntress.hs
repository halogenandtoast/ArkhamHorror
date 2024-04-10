module Arkham.Asset.Cards.GreenManMedallionHourOfTheHuntress (
  greenManMedallionHourOfTheHuntress,
  GreenManMedallionHourOfTheHuntress (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.DeckBuilding.Adjustment
import Arkham.Helpers.Investigator (eliminationWindow)
import Arkham.Projection
import Arkham.Token

newtype GreenManMedallionHourOfTheHuntress = GreenManMedallionHourOfTheHuntress AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenManMedallionHourOfTheHuntress :: AssetCard GreenManMedallionHourOfTheHuntress
greenManMedallionHourOfTheHuntress = asset GreenManMedallionHourOfTheHuntress Cards.greenManMedallionHourOfTheHuntress

instance HasAbilities GreenManMedallionHourOfTheHuntress where
  getAbilities (GreenManMedallionHourOfTheHuntress attrs) =
    controlledAbility
      attrs
      1
      (youExist can.spend.resources)
      (FastAbility $ exhaust attrs <> UpTo 3 (ResourceCost 1))
      : [ restrictedAbility attrs 2 ControlsThis $ ReactionAbility (eliminationWindow controller) Free
        | controller <- toList (assetController attrs)
        ]

resourcesPaid :: Payment -> Int
resourcesPaid (ResourcePayment n) = n
resourcesPaid (Payments ps) = sum $ map resourcesPaid ps
resourcesPaid _ = 0

instance RunMessage GreenManMedallionHourOfTheHuntress where
  runMessage msg a@(GreenManMedallionHourOfTheHuntress attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (resourcesPaid -> n) -> do
      placeTokens (attrs.ability 1) attrs Offering n
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- fieldMap AssetTokens (countTokens Offering) attrs.id
      let x = n `div` 6
      push $ AddDeckBuildingAdjustment iid (ReduceXpCostOfNextCardYouPurchaseBy x)
      pure a
    _ -> GreenManMedallionHourOfTheHuntress <$> lift (runMessage msg attrs)
