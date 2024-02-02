module Arkham.Location.Cards.BilliardsRoomSpectral (
  billiardsRoomSpectral,
  BilliardsRoomSpectral (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype BilliardsRoomSpectral = BilliardsRoomSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

billiardsRoomSpectral :: LocationCard BilliardsRoomSpectral
billiardsRoomSpectral =
  location BilliardsRoomSpectral Cards.billiardsRoomSpectral 3 (PerPlayer 1)

instance HasAbilities BilliardsRoomSpectral where
  getAbilities (BilliardsRoomSpectral attrs) =
    withBaseAbilities
      attrs
      [ haunted
          "You must either discard an asset you control or take 1 damage."
          attrs
          1
      ]

instance RunMessage BilliardsRoomSpectral where
  runMessage msg l@(BilliardsRoomSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      assets <- selectList $ assetControlledBy iid <> DiscardableAsset
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Take 1 damage"
          [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
        : [ Label
            "Discard an asset"
            [ChooseAndDiscardAsset iid (toSource attrs) AnyAsset]
          | notNull assets
          ]
      pure l
    _ -> BilliardsRoomSpectral <$> runMessage msg attrs
