module Arkham.Asset.Cards.SongOfTheDead2 (songOfTheDead2, songOfTheDead2Effect, SongOfTheDead2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Effect.Runner
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = asset SongOfTheDead2 Cards.songOfTheDead2

instance HasAbilities SongOfTheDead2 where
  getAbilities (SongOfTheDead2 x) = [restrictedAbility x 1 ControlsThis $ fightAction (assetUseCost x Charge 1)]

instance RunMessage SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (attrs.ability 1) iid (SkillModifier #willpower 1)
        , createCardEffect Cards.songOfTheDead2 Nothing (attrs.ability 1) iid
        , chooseFightEnemy iid (attrs.ability 1) #willpower
        ]
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
    RevealChaosToken _ iid token@(ChaosToken _ Skull) | InvestigatorTarget iid == effectTarget -> do
      push
        $ If
          (Window.RevealChaosTokenEffect iid token effectId)
          [skillTestModifier attrs effectTarget (DamageDealt 2)]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> SongOfTheDead2Effect <$> runMessage msg attrs
