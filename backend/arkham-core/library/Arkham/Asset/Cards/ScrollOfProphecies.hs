module Arkham.Asset.Cards.ScrollOfProphecies (
  ScrollOfProphecies (..),
  scrollOfProphecies,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype ScrollOfProphecies = ScrollOfProphecies AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfProphecies :: AssetCard ScrollOfProphecies
scrollOfProphecies = asset ScrollOfProphecies Cards.scrollOfProphecies

instance HasAbilities ScrollOfProphecies where
  getAbilities (ScrollOfProphecies x) =
    [ restrictedAbility x 1 ControlsThis
        $ ActionAbility []
        $ ActionCost 1
        <> assetUseCost x Secret 1
    ]

instance RunMessage ScrollOfProphecies where
  runMessage msg a@(ScrollOfProphecies attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      investigatorIds <- selectList $ affectsOthers $ colocatedWith iid
      investigators <- forToSnd investigatorIds $ \i -> drawCards i (toAbilitySource attrs 1) 3
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel iid'
            $ [drawing, toMessage $ chooseAndDiscardCard iid' (toAbilitySource attrs 1)]
          | (iid', drawing) <- investigators
          ]
      pure a
    _ -> ScrollOfProphecies <$> runMessage msg attrs
