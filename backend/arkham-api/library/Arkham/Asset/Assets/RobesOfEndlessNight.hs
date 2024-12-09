module Arkham.Asset.Assets.RobesOfEndlessNight (robesOfEndlessNight, RobesOfEndlessNight (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype RobesOfEndlessNight = RobesOfEndlessNight AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robesOfEndlessNight :: AssetCard RobesOfEndlessNight
robesOfEndlessNight =
  assetWith RobesOfEndlessNight Cards.robesOfEndlessNight (healthL ?~ 2)

instance HasModifiersFor RobesOfEndlessNight where
  getModifiersFor (RobesOfEndlessNight a) = controllerGetsWhen a a.ready [CanReduceCostOf #spell 1]

instance HasAbilities RobesOfEndlessNight where
  getAbilities (RobesOfEndlessNight a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard #when You (basic #spell)) (exhaust a)
    ]

instance RunMessage RobesOfEndlessNight where
  runMessage msg a@(RobesOfEndlessNight attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifier attrs iid $ ReduceCostOf (CardWithId card.id) 1
      pure a
    _ -> RobesOfEndlessNight <$> liftRunMessage msg attrs
