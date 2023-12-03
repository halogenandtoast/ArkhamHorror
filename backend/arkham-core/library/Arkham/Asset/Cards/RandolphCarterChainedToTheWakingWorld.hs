module Arkham.Asset.Cards.RandolphCarterChainedToTheWakingWorld (
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
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

randolphCarterChainedToTheWakingWorld :: AssetCard RandolphCarterChainedToTheWakingWorld
randolphCarterChainedToTheWakingWorld =
  asset RandolphCarterChainedToTheWakingWorld Cards.randolphCarterChainedToTheWakingWorld

instance HasModifiersFor RandolphCarterChainedToTheWakingWorld where
  getModifiersFor (InvestigatorTarget iid) (RandolphCarterChainedToTheWakingWorld a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #willpower 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities RandolphCarterChainedToTheWakingWorld where
  getAbilities (RandolphCarterChainedToTheWakingWorld attrs) =
    [ restrictedAbility attrs 1 (ControlsThis <> DuringSkillTest SkillTestAtYourLocation)
        $ ReactionAbility (RevealChaosToken #after You #elderthing) (exhaust attrs)
    ]

instance RunMessage RandolphCarterChainedToTheWakingWorld where
  runMessage msg a@(RandolphCarterChainedToTheWakingWorld attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ drawCards iid (attrs.ability 1) 2
      pure a
    _ -> RandolphCarterChainedToTheWakingWorld <$> runMessage msg attrs
