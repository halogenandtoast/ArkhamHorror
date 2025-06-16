module Arkham.Asset.Assets.ElizabethConrad (
  elizabethConrad,
  ElizabethConrad(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Matcher

newtype ElizabethConrad = ElizabethConrad AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elizabethConrad :: AssetCard ElizabethConrad
elizabethConrad = allyWith ElizabethConrad Cards.elizabethConrad (1, 3) noSlots

instance HasAbilities ElizabethConrad where
  getAbilities (ElizabethConrad a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility
            (DrawsCards #after You AnyCards AnyValue <> DuringTurn You)
            (exhaust a)
    ]

instance RunMessage ElizabethConrad where
  runMessage msg a@(ElizabethConrad attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ colocatedWith iid
      chooseOne iid $ targetLabels investigators $ \iid' -> do
        locations <- getAccessibleLocations iid' attrs
        chooseOne iid' $ targetLabels locations $ \lid ->
          Move $ move (attrs.ability 1) iid' lid
      pure a
    _ -> ElizabethConrad <$> liftRunMessage msg attrs
