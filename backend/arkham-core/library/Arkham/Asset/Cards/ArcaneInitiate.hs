module Arkham.Asset.Cards.ArcaneInitiate (
  arcaneInitiate,
  ArcaneInitiate (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetCard ArcaneInitiate
arcaneInitiate = ally ArcaneInitiate Cards.arcaneInitiate (1, 2)

instance HasAbilities ArcaneInitiate where
  getAbilities (ArcaneInitiate a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ AssetEntersPlay Timing.When
        $ AssetWithId (toId a)
    , restrictedAbility a 2 ControlsThis
        $ FastAbility
        $ exhaust a
    ]

instance RunMessage ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push
        $ chooseOne iid
        $ [ targetLabel iid
              $ [ Search
                    iid
                    (toSource attrs)
                    (toTarget iid)
                    [fromTopOfDeck 3]
                    (CardWithTrait Spell)
                    $ DrawFound iid 1
                ]
          ]
      pure a
    _ -> ArcaneInitiate <$> runMessage msg attrs
