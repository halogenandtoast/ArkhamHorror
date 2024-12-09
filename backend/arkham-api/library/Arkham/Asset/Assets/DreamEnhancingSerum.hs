module Arkham.Asset.Assets.DreamEnhancingSerum (
  dreamEnhancingSerum,
  DreamEnhancingSerum (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Matcher

newtype DreamEnhancingSerum = DreamEnhancingSerum AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamEnhancingSerum :: AssetCard DreamEnhancingSerum
dreamEnhancingSerum = asset DreamEnhancingSerum Cards.dreamEnhancingSerum

instance HasModifiersFor DreamEnhancingSerum where
  getModifiersFor (DreamEnhancingSerum a) = controllerGets a [OnlyFirstCopyCardCountsTowardMaximumHandSize]

-- TODO: No good way to handle reveal
instance HasAbilities DreamEnhancingSerum where
  getAbilities (DreamEnhancingSerum a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DrawCard #after (You <> can.draw.cards) (CardWithCopyInHand You) AnyDeck)
          (exhaust a)
    ]

instance RunMessage DreamEnhancingSerum where
  runMessage msg a@(DreamEnhancingSerum attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> DreamEnhancingSerum <$> runMessage msg attrs
