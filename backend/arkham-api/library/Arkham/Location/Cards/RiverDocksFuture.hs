module Arkham.Location.Cards.RiverDocksFuture (riverDocksFuture) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Scientist))

newtype RiverDocksFuture = RiverDocksFuture LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverDocksFuture :: LocationCard RiverDocksFuture
riverDocksFuture = location RiverDocksFuture Cards.riverDocksFuture 1 (PerPlayer 1)

instance HasAbilities RiverDocksFuture where
  getAbilities (RiverDocksFuture a) =
    extendRevealed
      a
      [ restricted a 1 Here $ FastAbility (SpendTokenCost Token.Shipment (TargetIs $ toTarget a))
      , groupLimit PerRound
          $ restricted a 2 Here
          $ FastAbility
          $ OrCost
            [ ResourceCost 3
            , ExhaustAssetCost (AssetWithTrait Scientist <> AssetAt (be a))
                <> ExhaustAssetCost (AssetWithTrait Scientist <> AssetAt (be a))
            ]
      ]

instance RunMessage RiverDocksFuture where
  runMessage msg l@(RiverDocksFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 4 $ labeled' "gainResources" $ gainResources iid (attrs.ability 1) 4
        countVar 1 $ labeled' "gainClues" $ gainClues iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      docks <- selectOne $ locationIs Cards.riverDocksPresent
      for_ docks \dock -> placeTokens (attrs.ability 2) dock Token.Shipment 1
      pure l
    _ -> RiverDocksFuture <$> liftRunMessage msg attrs
