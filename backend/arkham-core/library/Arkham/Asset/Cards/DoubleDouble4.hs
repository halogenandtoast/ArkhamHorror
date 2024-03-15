module Arkham.Asset.Cards.DoubleDouble4 (
  doubleDouble4,
  DoubleDouble4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Cost
import Arkham.Matcher

newtype DoubleDouble4 = DoubleDouble4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleDouble4 :: AssetCard DoubleDouble4
doubleDouble4 = asset DoubleDouble4 Cards.doubleDouble4

instance HasAbilities DoubleDouble4 where
  getAbilities (DoubleDouble4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (PlayCard #after You $ #event <> CardWithoutModifier RemoveFromGameInsteadOfDiscard)
          (exhaust a)
    ]

instance RunMessage DoubleDouble4 where
  runMessage msg a@(DoubleDouble4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(cardPlayed -> card) _ -> do
      pushAll [AddToHand iid [card], PutCardIntoPlay iid card Nothing NoPayment ws]
      pure a
    _ -> DoubleDouble4 <$> runMessage msg attrs
