module Arkham.Asset.Assets.JessicaHyde1 (jessicaHyde1, JessicaHyde1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype JessicaHyde1 = JessicaHyde1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jessicaHyde1 :: AssetCard JessicaHyde1
jessicaHyde1 =
  allyWith JessicaHyde1 Cards.jessicaHyde1 (3, 1) (tokensL %~ addTokens #damage 2)

instance HasModifiersFor JessicaHyde1 where
  getModifiersFor (JessicaHyde1 a) = controllerGets a [SkillModifier #combat 1]

instance HasAbilities JessicaHyde1 where
  getAbilities (JessicaHyde1 x) =
    [ controlledAbility
        x
        1
        (exists $ HealableAsset (toSource x) #damage $ AssetWithId (toId x))
        (freeReaction (TurnEnds #after You))
    ]

instance RunMessage JessicaHyde1 where
  runMessage msg a@(JessicaHyde1 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealDamage (toTarget attrs) (toSource attrs) 1
      pure a
    _ -> JessicaHyde1 <$> runMessage msg attrs
