module Arkham.Asset.Cards.ShardsOfTheVoid3 (
  shardsOfTheVoid3,
  ShardsOfTheVoid3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Projection
import Arkham.SkillType

newtype ShardsOfTheVoid3 = ShardsOfTheVoid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shardsOfTheVoid3 :: AssetCard ShardsOfTheVoid3
shardsOfTheVoid3 = asset ShardsOfTheVoid3 Cards.shardsOfTheVoid3

instance HasAbilities ShardsOfTheVoid3 where
  getAbilities (ShardsOfTheVoid3 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Fight) $
          OrCost [UseCost (AssetWithId $ toId a) Charge 1, ReleaseChaosTokensCost 1]
    ]

instance RunMessage ShardsOfTheVoid3 where
  runMessage msg a@(ShardsOfTheVoid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      zeros <-
        fieldMap
          AssetSealedChaosTokens
          (length . filter ((== Zero) . chaosTokenFace))
          (toId attrs)
      pushAll
        [ skillTestModifiers attrs iid $
            DamageDealt 1 : [SkillModifier SkillWillpower (zeros * 2) | zeros > 0]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillWillpower mempty False
        ]
      pure a
    RevealChaosToken (SkillTestSource iid _ (isSource attrs -> True) _) _ token | chaosTokenFace token == Zero -> do
      pushAll
        [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
        , SealChaosToken token
        , SealedChaosToken token (toCard attrs)
        ]
      pure a
    _ -> ShardsOfTheVoid3 <$> runMessage msg attrs
