module Arkham.Asset.Cards.DoubleDouble4 (
  doubleDouble4,
  DoubleDouble4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DoubleDouble4 = DoubleDouble4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleDouble4 :: AssetCard DoubleDouble4
doubleDouble4 = asset DoubleDouble4 Cards.doubleDouble4

instance HasAbilities DoubleDouble4 where
  getAbilities (DoubleDouble4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( PlayCard
              Timing.After
              You
              (BasicCardMatch (CardWithType EventType))
          )
          (ExhaustCost $ toTarget a)
    ]

getCardPlayed :: [Window] -> Card
getCardPlayed [] = error "missing play card window"
getCardPlayed ((windowType -> Window.PlayCard _ c) : _) = c
getCardPlayed (_ : xs) = getCardPlayed xs

instance RunMessage DoubleDouble4 where
  runMessage msg a@(DoubleDouble4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(getCardPlayed -> card) _ -> do
      pushAll [AddToHand iid [card], PutCardIntoPlay iid card Nothing ws]
      pure a
    _ -> DoubleDouble4 <$> runMessage msg attrs
