module Arkham.Asset.Assets.GravediggersShovel (gravediggersShovel) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Fight
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype GravediggersShovel = GravediggersShovel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravediggersShovel :: AssetCard GravediggersShovel
gravediggersShovel = asset GravediggersShovel Cards.gravediggersShovel

instance HasAbilities GravediggersShovel where
  getAbilities (GravediggersShovel x) =
    [ fightAbility x 1 mempty ControlsThis
    , restricted x 2 (ControlsThis <> youExist (InvestigatorCanDiscoverCluesAt YourLocation))
        $ actionAbilityWithCost (discardCost x)
    ]

instance RunMessage GravediggersShovel where
  runMessage msg a@(GravediggersShovel attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (SkillModifier #combat 2)
      pushAll [enabled, chooseFight]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (attrs.ability 2) 1
      pure a
    _ -> GravediggersShovel <$> runMessage msg attrs
