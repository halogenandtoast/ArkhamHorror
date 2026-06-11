module Arkham.Location.Cards.RiverDocksPresent (riverDocksPresent) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype RiverDocksPresent = RiverDocksPresent LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksPresent :: LocationCard RiverDocksPresent
riverDocksPresent = location RiverDocksPresent Cards.riverDocksPresent 1 (PerPlayer 1)

instance HasAbilities RiverDocksPresent where
  getAbilities (RiverDocksPresent a) =
    extendRevealed
      a
      [ restricted a 1 Here $ FastAbility (SpendTokenCost Token.Shipment (TargetIs $ toTarget a))
      , restricted a 2 Here
          $ actionAbilityWithCost
          $ OrCost
            [ ResourceCost 3
            , ExhaustAssetCost (AssetWithTrait Scientist <> AssetAt (be a))
                <> ExhaustAssetCost (AssetWithTrait Scientist <> AssetAt (be a))
            ]
      ]

instance RunMessage RiverDocksPresent where
  runMessage msg l@(RiverDocksPresent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 4 $ labeled' "gainResources" $ gainResources iid (attrs.ability 1) 4
        countVar 1 $ labeled' "gainClues" $ gainClues iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      docks <- selectOne $ locationIs Cards.riverDocksFuture
      for_ docks \dock -> placeTokens (attrs.ability 2) dock Token.Shipment 1
      pure l
    _ -> RiverDocksPresent <$> liftRunMessage msg attrs
