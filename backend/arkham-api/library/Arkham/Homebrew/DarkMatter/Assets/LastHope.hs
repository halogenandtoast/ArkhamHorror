module Arkham.Homebrew.DarkMatter.Assets.LastHope (lastHope) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Matcher
import Arkham.Placement

{- | One of Starfall's three escape objectives: an investigator at Hope must
control Shielding Device, which is then removed from the game and paid for with
2[per_investigator] clues as a group.
-}
newtype LastHope = LastHope AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastHope :: AssetCard LastHope
lastHope = asset LastHope Cards.lastHope

instance HasAbilities LastHope where
  getAbilities (LastHope a) =
    [ restricted
        a
        1
        ( exists
            $ assetIs Cards.shieldingDevice
            <> AssetControlledBy (InvestigatorAt $ locationIs Locations.hope)
        )
        $ Objective
        $ FastAbility
        $ GroupClueCost (PerPlayer 2) Anywhere
    ]

instance RunMessage LastHope where
  runMessage msg a@(LastHope attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      push $ PlaceAsset attrs.id NextToAct
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (assetIs Cards.shieldingDevice) >>= traverse_ (push . RemoveFromGame . toTarget)
      addToVictory iid attrs
      pure a
    _ -> LastHope <$> liftRunMessage msg attrs
