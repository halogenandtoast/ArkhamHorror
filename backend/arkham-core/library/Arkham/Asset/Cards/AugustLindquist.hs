module Arkham.Asset.Cards.AugustLindquist (
  augustLindquist,
  AugustLindquist (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Id
import Arkham.Matcher

newtype AugustLindquist = AugustLindquist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augustLindquist :: AssetCard AugustLindquist
augustLindquist =
  asset AugustLindquist Cards.augustLindquist

instance HasAbilities AugustLindquist where
  getAbilities (AugustLindquist attrs) =
    [ restrictedAbility attrs 1 OnSameLocation
        $ ActionAbility (Just Action.Parley)
        $ ActionCost 1
        <> GroupClueCost (PerPlayer 2) (locationWithAsset $ toId attrs)
    ]

getSpentClues :: Payment -> [InvestigatorId]
getSpentClues (Payments xs) = concatMap getSpentClues xs
getSpentClues (CluePayment iid _) = [iid]
getSpentClues _ = []

instance RunMessage AugustLindquist where
  runMessage msg a@(AugustLindquist attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getSpentClues -> spentClues) -> do
      spentCluesWithPlayer <- traverse (traverseToSnd getPlayer) spentClues
      pushAll
        $ [ chooseOne
            player
            [ Label "Take 1 damage" [InvestigatorAssignDamage iid' (toSource attrs) DamageAny 1 0]
            , Label "Take 1 horror" [InvestigatorAssignDamage iid' (toSource attrs) DamageAny 0 1]
            ]
          | (iid', player) <- spentCluesWithPlayer
          ]
        <> [RemoveFromGame (toTarget attrs)]
        <> [PlaceKey (toTarget iid) k | k <- toList (assetKeys attrs)]

      pure a
    _ -> AugustLindquist <$> runMessage msg attrs
