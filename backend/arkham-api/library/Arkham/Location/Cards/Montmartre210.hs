module Arkham.Location.Cards.Montmartre210 (
  montmartre210,
  Montmartre210 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype Montmartre210 = Montmartre210 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

montmartre210 :: LocationCard Montmartre210
montmartre210 = location Montmartre210 Cards.montmartre210 2 (PerPlayer 1)

instance HasAbilities Montmartre210 where
  getAbilities (Montmartre210 attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (PlayerLimit PerRound 1)
        $ restrictedAbility
          attrs
          1
          ( Here
              <> AssetExists
                ( AssetControlledBy You
                    <> AssetOneOf [AssetWithUses Ammo, AssetWithUses Supply]
                )
          )
        $ ActionAbility []
        $ ActionCost 1
        <> ResourceCost 1
      | locationRevealed attrs
      ]

instance RunMessage Montmartre210 where
  runMessage msg a@(Montmartre210 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      ammoAssets <- select $ AssetControlledBy You <> AssetWithUses Ammo
      supplyAssets <- select $ AssetControlledBy You <> AssetWithUses Supply
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [targetLabel asset [AddUses (attrs.ability 1) asset Ammo 1] | asset <- ammoAssets]
        <> [ targetLabel asset [AddUses (attrs.ability 1) asset Supply 1]
           | asset <- supplyAssets
           ]
      pure a
    _ -> Montmartre210 <$> runMessage msg attrs
