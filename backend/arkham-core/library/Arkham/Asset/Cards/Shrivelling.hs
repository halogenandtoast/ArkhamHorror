module Arkham.Asset.Cards.Shrivelling (
  Shrivelling (..),
  shrivelling,
  shrivellingEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Window qualified as Window

newtype Shrivelling = Shrivelling AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

shrivelling :: AssetCard Shrivelling
shrivelling = asset Shrivelling Cards.shrivelling

instance HasAbilities Shrivelling where
  getAbilities (Shrivelling a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbilityWithSkill [#fight] #willpower
        $ ActionCost 1
        <> assetUseCost a Charge 1
    ]

instance RunMessage Shrivelling where
  runMessage msg a@(Shrivelling attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ skillTestModifier source iid (DamageDealt 1)
        , createCardEffect Cards.shrivelling Nothing source iid
        , chooseFightEnemy iid source #willpower
        ]
      pure a
    _ -> Shrivelling <$> runMessage msg attrs

newtype ShrivellingEffect = ShrivellingEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

shrivellingEffect :: EffectArgs -> ShrivellingEffect
shrivellingEffect = cardEffect ShrivellingEffect Cards.shrivelling

instance RunMessage ShrivellingEffect where
  runMessage msg e@(ShrivellingEffect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      let horror = maybe 1 intFromMetadata attrs.metadata
      when (chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        $ pushAll
          [ If (Window.RevealChaosTokenEffect iid token (toId attrs)) [assignHorror iid attrs.source horror]
          , disable attrs
          ]
      pure e
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> ShrivellingEffect <$> runMessage msg attrs
