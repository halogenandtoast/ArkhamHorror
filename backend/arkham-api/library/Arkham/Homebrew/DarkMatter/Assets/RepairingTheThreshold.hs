module Arkham.Homebrew.DarkMatter.Assets.RepairingTheThreshold (repairingTheThreshold) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three escape objectives: an investigator at Threshold of Yuggoth must
control Stasis Cube, which is then removed from the game and paid for with
2[per_investigator] clues as a group.
-}
newtype RepairingTheThreshold = RepairingTheThreshold AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

repairingTheThreshold :: AssetCard RepairingTheThreshold
repairingTheThreshold = asset RepairingTheThreshold Cards.repairingTheThreshold

instance HasAbilities RepairingTheThreshold where
  getAbilities (RepairingTheThreshold a) =
    [ restricted
        a
        1
        ( exists
            $ assetIs Cards.stasisCube
            <> AssetControlledBy (InvestigatorAt $ locationIs Locations.thresholdOfYuggoth)
        )
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage RepairingTheThreshold where
  runMessage msg a@(RepairingTheThreshold attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      push $ PlaceAsset attrs.id NextToAct
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (assetIs Cards.stasisCube) >>= traverse_ (push . RemoveFromGame . toTarget)
      addToVictory iid attrs
      pure a
    _ -> RepairingTheThreshold <$> liftRunMessage msg attrs
