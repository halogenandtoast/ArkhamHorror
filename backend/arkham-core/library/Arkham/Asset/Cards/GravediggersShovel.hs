module Arkham.Asset.Cards.GravediggersShovel (
  gravediggersShovel,
  GravediggersShovel (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Discover
import Arkham.Matcher

newtype GravediggersShovel = GravediggersShovel AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

gravediggersShovel :: AssetCard GravediggersShovel
gravediggersShovel = asset GravediggersShovel Cards.gravediggersShovel

instance HasAbilities GravediggersShovel where
  getAbilities (GravediggersShovel x) =
    [ fightAbility x 1 mempty ControlsThis
    , restrictedAbility
        x
        2
        (ControlsThis <> InvestigatorExists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ actionAbilityWithCost (discardCost x)
    ]

instance RunMessage GravediggersShovel where
  runMessage msg a@(GravediggersShovel attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier attrs iid (SkillModifier #combat 2)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 2) 1
      pure a
    _ -> GravediggersShovel <$> runMessage msg attrs
