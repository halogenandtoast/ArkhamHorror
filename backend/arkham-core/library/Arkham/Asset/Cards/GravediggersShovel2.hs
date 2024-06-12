module Arkham.Asset.Cards.GravediggersShovel2 (gravediggersShovel2, GravediggersShovel2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Fight
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Placement
import Arkham.Prelude

newtype GravediggersShovel2 = GravediggersShovel2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravediggersShovel2 :: AssetCard GravediggersShovel2
gravediggersShovel2 = asset GravediggersShovel2 Cards.gravediggersShovel2

instance HasAbilities GravediggersShovel2 where
  getAbilities (GravediggersShovel2 x) =
    [ fightAbility x 1 mempty ControlsThis
    , restrictedAbility x 2 (ControlsThis <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
        $ actionAbilityWithCost (OrCost [discardCost x, removeCost x])
    ]

instance RunMessage GravediggersShovel2 where
  runMessage msg a@(GravediggersShovel2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll [skillTestModifier source iid (SkillModifier #combat 2), chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let
        n =
          case attrs.placement of
            OutOfPlay RemovedZone -> 2
            _ -> 1
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (attrs.ability 2) n
      pure a
    _ -> GravediggersShovel2 <$> runMessage msg attrs
