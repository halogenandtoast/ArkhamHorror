module Arkham.Asset.Assets.RobesOfEndlessNight2 (robesOfEndlessNight2, RobesOfEndlessNight2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher

newtype RobesOfEndlessNight2 = RobesOfEndlessNight2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

robesOfEndlessNight2 :: AssetCard RobesOfEndlessNight2
robesOfEndlessNight2 =
  assetWith RobesOfEndlessNight2 Cards.robesOfEndlessNight2 (healthL ?~ 2)

instance HasModifiersFor RobesOfEndlessNight2 where
  getModifiersFor (RobesOfEndlessNight2 a) = controllerGetsWhen a a.ready [CanReduceCostOf #spell 1]

instance HasAbilities RobesOfEndlessNight2 where
  getAbilities (RobesOfEndlessNight2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard #when You (basic #spell)) (exhaust a)
    ]

instance RunMessage RobesOfEndlessNight2 where
  runMessage msg a@(RobesOfEndlessNight2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      costModifiers
        attrs
        iid
        [ ReduceCostOf (CardWithId card.id) 1
        , ActionDoesNotCauseAttacksOfOpportunity #play
        ]
      pure a
    _ -> RobesOfEndlessNight2 <$> liftRunMessage msg attrs
