module Arkham.Asset.Cards.BorrowedTime3 (
  borrowedTime3,
  BorrowedTime3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype BorrowedTime3 = BorrowedTime3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

borrowedTime3 :: AssetCard BorrowedTime3
borrowedTime3 = asset BorrowedTime3 Cards.borrowedTime3

instance HasAbilities BorrowedTime3 where
  getAbilities (BorrowedTime3 a) =
    doesNotProvokeAttacksOfOpportunity
      ( restrictedAbility a 1 ControlsThis
          $ ActionAbility []
          $ ActionCost
            1
      )
      : [ restrictedAbility a 2 ControlsThis
            $ ForcedAbility
            $ TurnBegins Timing.When
            $ maybe NoOne InvestigatorWithId (a ^. controllerL)
        ]

instance RunMessage BorrowedTime3 where
  runMessage msg a@(BorrowedTime3 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceResources (toAbilitySource attrs 1) (toTarget attrs) 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      let n = assetResources attrs
      pushAll
        [ RemoveResources (toAbilitySource attrs 2) (toTarget attrs) n
        , GainActions iid (toAbilitySource attrs 2) n
        ]
      pure a
    _ -> BorrowedTime3 <$> runMessage msg attrs
