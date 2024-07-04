module Arkham.Asset.Cards.ShardsOfTheVoid3 (shardsOfTheVoid3, ShardsOfTheVoid3 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Fight
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier

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
        (OrCost [assetUseCost a Charge 1, ReleaseChaosTokensCost 1 #any])
        ControlsThis
    ]

instance RunMessage ShardsOfTheVoid3 where
  runMessage msg a@(ShardsOfTheVoid3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      skillTestModifiers source iid $ DamageDealt 1
        : [ ForEach
              (CountChaosTokens $ SealedOnAsset (be attrs) (ChaosTokenFaceIs Zero))
              [SkillModifier #willpower 2]
          ]
      chooseFight <- aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight iid source)
      pushAll $ leftOr chooseFight
      pure a
    Msg.RevealChaosToken SkillTestSource _ token | chaosTokenFace token == Zero -> do
      mSource <- getSkillTestSource
      mInvestigator <- getSkillTestInvestigator
      for_ ((,) <$> mSource <*> mInvestigator) $ \(source, iid) -> do
        when (isAbilitySource attrs 1 source) do
          skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
          pushAll
            [ SealChaosToken token
            , SealedChaosToken token (toCard attrs)
            ]
      pure a
    _ -> ShardsOfTheVoid3 <$> liftRunMessage msg attrs
