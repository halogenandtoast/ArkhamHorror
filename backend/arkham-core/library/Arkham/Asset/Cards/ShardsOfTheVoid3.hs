module Arkham.Asset.Cards.ShardsOfTheVoid3
  ( shardsOfTheVoid3
  , ShardsOfTheVoid3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Token

newtype ShardsOfTheVoid3 = ShardsOfTheVoid3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shardsOfTheVoid3 :: AssetCard ShardsOfTheVoid3
shardsOfTheVoid3 = asset ShardsOfTheVoid3 Cards.shardsOfTheVoid3

instance HasAbilities ShardsOfTheVoid3 where
  getAbilities (ShardsOfTheVoid3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ OrCost [UseCost (AssetWithId $ toId a) Charge 1, ReleaseTokensCost 1]
    ]

instance RunMessage ShardsOfTheVoid3 where
  runMessage msg a@(ShardsOfTheVoid3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      zeros <- fieldMap
        AssetSealedTokens
        (length . filter ((== Zero) . tokenFace))
        (toId attrs)
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          (DamageDealt 1
          : [ SkillModifier SkillWillpower (zeros * 2) | zeros > 0 ]
          )
        , ChooseFightEnemy
          iid
          (toSource attrs)
          Nothing
          SkillWillpower
          mempty
          False
        ]
      pure a
    RevealToken (SkillTestSource iid _ (isSource attrs -> True) _) _ token
      | tokenFace token == Zero -> do
        pushAll
          [ skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1)
          , SealToken token
          , SealedToken token (toCard attrs)
          ]
        pure a
    _ -> ShardsOfTheVoid3 <$> runMessage msg attrs
