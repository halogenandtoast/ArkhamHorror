module Arkham.Location.Cards.ArkhamAdvertiserPresent (arkhamAdvertiserPresent) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype ArkhamAdvertiserPresent = ArkhamAdvertiserPresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamAdvertiserPresent :: LocationCard ArkhamAdvertiserPresent
arkhamAdvertiserPresent =
  location ArkhamAdvertiserPresent Cards.arkhamAdvertiserPresent 2 (PerPlayer 1)

instance HasAbilities ArkhamAdvertiserPresent where
  getAbilities (ArkhamAdvertiserPresent a) =
    extendRevealed
      a
      [ groupLimit PerRound
          $ restricted a 1 (Here <> exists (AssetWithTrait Scientist <> AssetAt (be a) <> AssetExhausted))
          $ FastAbility (ResourceCost 1)
      , restricted a 2 Here
          $ actionAbilityWithCost (SpendTokenCost Token.Newspaper (TargetIs $ toTarget a))
      ]

instance RunMessage ArkhamAdvertiserPresent where
  runMessage msg l@(ArkhamAdvertiserPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scientists <- select $ AssetWithTrait Scientist <> AssetAt (be attrs) <> AssetExhausted
      chooseTargetM iid scientists \scientist -> readyThis scientist
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      placeClues (attrs.ability 2) attrs n
      gainActions iid (attrs.ability 2) 4
      pure l
    _ -> ArkhamAdvertiserPresent <$> liftRunMessage msg attrs
