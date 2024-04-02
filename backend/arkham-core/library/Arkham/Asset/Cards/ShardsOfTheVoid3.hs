module Arkham.Asset.Cards.ShardsOfTheVoid3 (shardsOfTheVoid3, ShardsOfTheVoid3 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection

newtype ShardsOfTheVoid3 = ShardsOfTheVoid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shardsOfTheVoid3 :: AssetCard ShardsOfTheVoid3
shardsOfTheVoid3 = asset ShardsOfTheVoid3 Cards.shardsOfTheVoid3

instance HasAbilities ShardsOfTheVoid3 where
  getAbilities (ShardsOfTheVoid3 a) =
    [ fightAbility
        a
        1
        (OrCost [assetUseCost a Charge 1, ReleaseChaosTokensCost 1])
        ControlsThis
    ]

instance RunMessage ShardsOfTheVoid3 where
  runMessage msg a@(ShardsOfTheVoid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      zeros <- fieldMap AssetSealedChaosTokens (count ((== Zero) . chaosTokenFace)) (toId attrs)
      let source = attrs.ability 1
      chooseFight <- aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll
        $ skillTestModifiers source iid (DamageDealt 1 : [SkillModifier #willpower (zeros * 2) | zeros > 0])
        : leftOr chooseFight
      pure a
    Msg.RevealChaosToken SkillTestSource _ token | chaosTokenFace token == Zero -> do
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      for_ ((,) <$> mSource <*> mInvestigator) $ \(source, iid) -> do
        when (isAbilitySource attrs 1 source)
          $ pushAll
            [ skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
            , SealChaosToken token
            , SealedChaosToken token (toCard attrs)
            ]
      pure a
    _ -> ShardsOfTheVoid3 <$> runMessage msg attrs
