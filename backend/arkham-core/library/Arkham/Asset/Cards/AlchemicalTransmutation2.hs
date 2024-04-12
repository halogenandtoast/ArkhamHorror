module Arkham.Asset.Cards.AlchemicalTransmutation2 (
  alchemicalTransmutation2,
  alchemicalTransmutation2Effect,
  AlchemicalTransmutation2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Window qualified as Window

newtype AlchemicalTransmutation2 = AlchemicalTransmutation2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation2 :: AssetCard AlchemicalTransmutation2
alchemicalTransmutation2 = asset AlchemicalTransmutation2 Cards.alchemicalTransmutation2

instance HasAbilities AlchemicalTransmutation2 where
  getAbilities (AlchemicalTransmutation2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage AlchemicalTransmutation2 where
  runMessage msg a@(AlchemicalTransmutation2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      pushAll
        [ createCardEffect Cards.alchemicalTransmutation2 Nothing source iid
        , beginSkillTest iid (attrs.ability 1) attrs #willpower (Fixed 0)
        ]
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) (min 4 -> n) -> do
      push $ TakeResources iid n (attrs.ability 1) False
      pure a
    _ -> AlchemicalTransmutation2 <$> runMessage msg attrs

newtype AlchemicalTransmutation2Effect = AlchemicalTransmutation2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alchemicalTransmutation2Effect :: EffectArgs -> AlchemicalTransmutation2Effect
alchemicalTransmutation2Effect = cardEffect AlchemicalTransmutation2Effect Cards.alchemicalTransmutation2

instance RunMessage AlchemicalTransmutation2Effect where
  runMessage msg e@(AlchemicalTransmutation2Effect attrs) =
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
      _ -> AlchemicalTransmutation2Effect <$> runMessage msg attrs
