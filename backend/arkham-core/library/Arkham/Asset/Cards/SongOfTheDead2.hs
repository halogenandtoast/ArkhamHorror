module Arkham.Asset.Cards.SongOfTheDead2 (songOfTheDead2, songOfTheDead2Effect, SongOfTheDead2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = asset SongOfTheDead2 Cards.songOfTheDead2

instance HasAbilities SongOfTheDead2 where
  getAbilities (SongOfTheDead2 x) =
    [restrictedAbility x 1 ControlsThis $ fightAction (assetUseCost x Charge 1)]

instance RunMessage SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #combat) (mkChooseFight sid iid source)
      pushAll
        $ [ skillTestModifier sid source iid (SkillModifier #willpower 1)
          , createCardEffect Cards.songOfTheDead2 (effectMetaTarget sid) source iid
          ]
        <> chooseFight
      pure a
    _ -> SongOfTheDead2 <$> runMessage msg attrs

newtype SongOfTheDead2Effect = SongOfTheDead2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2Effect :: EffectArgs -> SongOfTheDead2Effect
songOfTheDead2Effect = cardEffect SongOfTheDead2Effect Cards.songOfTheDead2

instance HasModifiersFor SongOfTheDead2Effect

instance RunMessage SongOfTheDead2Effect where
  runMessage msg e@(SongOfTheDead2Effect attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token | isTarget iid attrs.target && token.face == #skull -> do
      withSkillTest \sid ->
        push
          $ If
            (Window.RevealChaosTokenEffect iid token effectId)
            [skillTestModifier sid attrs effectTarget (DamageDealt 2)]
      pure e
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      push $ DisableEffect effectId
      pure e
    _ -> SongOfTheDead2Effect <$> runMessage msg attrs
