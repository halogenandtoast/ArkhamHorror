module Arkham.Asset.Assets.RandolphCarterChainedToTheWakingWorld (
  randolphCarterChainedToTheWakingWorld,
  RandolphCarterChainedToTheWakingWorld (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype RandolphCarterChainedToTheWakingWorld = RandolphCarterChainedToTheWakingWorld AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterChainedToTheWakingWorld :: AssetCard RandolphCarterChainedToTheWakingWorld
randolphCarterChainedToTheWakingWorld =
  ally RandolphCarterChainedToTheWakingWorld Cards.randolphCarterChainedToTheWakingWorld (2, 3)

instance HasModifiersFor RandolphCarterChainedToTheWakingWorld where
  getModifiersFor (RandolphCarterChainedToTheWakingWorld a) = controllerGets a [SkillModifier #willpower 1, SkillModifier #intellect 1]

instance HasAbilities RandolphCarterChainedToTheWakingWorld where
  getAbilities (RandolphCarterChainedToTheWakingWorld attrs) =
    [ restrictedAbility attrs 1 (ControlsThis <> DuringSkillTest SkillTestAtYourLocation)
        $ ReactionAbility (RevealChaosToken #after Anyone #elderthing) (exhaust attrs)
    ]

instance RunMessage RandolphCarterChainedToTheWakingWorld where
  runMessage msg a@(RandolphCarterChainedToTheWakingWorld attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawCards iid (attrs.ability 1) 2
      pure a
    _ -> RandolphCarterChainedToTheWakingWorld <$> runMessage msg attrs
