module Arkham.Asset.Cards.AlchemicalTransmutation (
  alchemicalTransmutation,
  alchemicalTransmutationEffect,
  AlchemicalTransmutation (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Window qualified as Window

newtype AlchemicalTransmutation = AlchemicalTransmutation AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

alchemicalTransmutation :: AssetCard AlchemicalTransmutation
alchemicalTransmutation = asset AlchemicalTransmutation Cards.alchemicalTransmutation

instance HasAbilities AlchemicalTransmutation where
  getAbilities (AlchemicalTransmutation a) =
    [ restrictedAbility a 1 ControlsThis
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage AlchemicalTransmutation where
  runMessage msg a@(AlchemicalTransmutation attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ createCardEffect Cards.alchemicalTransmutation Nothing source iid
        , beginSkillTest iid source attrs #willpower 1
        ]
      pure a
    PassedThisSkillTestBy iid (isSource attrs -> True) (min 3 -> n) -> do
      push $ TakeResources iid n (toAbilitySource attrs 1) False
      pure a
    _ -> AlchemicalTransmutation <$> runMessage msg attrs

newtype AlchemicalTransmutationEffect = AlchemicalTransmutationEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

alchemicalTransmutationEffect :: EffectArgs -> AlchemicalTransmutationEffect
alchemicalTransmutationEffect = cardEffect AlchemicalTransmutationEffect Cards.alchemicalTransmutation

instance RunMessage AlchemicalTransmutationEffect where
  runMessage msg e@(AlchemicalTransmutationEffect attrs) =
    case msg of
      RevealChaosToken _ iid token | toTarget iid == effectTarget attrs -> do
        let triggers = chaosTokenFace token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]
        when triggers $ do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token (toId attrs))
                [assignDamage iid (effectSource attrs) 1]
            , DisableEffect $ toId attrs
            ]
        pure e
      SkillTestEnds _ _ -> do
        push $ DisableEffect $ toId attrs
        pure e
      _ -> AlchemicalTransmutationEffect <$> runMessage msg attrs
