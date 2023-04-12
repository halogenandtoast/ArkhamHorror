module Arkham.Asset.Cards.EsotericAtlas1
  ( esotericAtlas1
  , EsotericAtlas1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Movement

newtype EsotericAtlas1 = EsotericAtlas1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericAtlas1 :: AssetCard EsotericAtlas1
esotericAtlas1 = asset EsotericAtlas1 Cards.esotericAtlas1

instance HasAbilities EsotericAtlas1 where
  getAbilities (EsotericAtlas1 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> LocationExists
            (LocationWithDistanceFrom 2 RevealedLocation)
          )
        $ ActionAbility Nothing
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Secret 1
    ]

instance RunMessage EsotericAtlas1 where
  runMessage msg a@(EsotericAtlas1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locations <- selectList $ LocationWithDistanceFrom 2 RevealedLocation
      push $ chooseOne
        iid
        [ targetLabel location [MoveTo $ move (toSource attrs) iid location]
        | location <- locations
        ]
      pure a
    _ -> EsotericAtlas1 <$> runMessage msg attrs
