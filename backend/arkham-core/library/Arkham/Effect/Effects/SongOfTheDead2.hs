module Arkham.Effect.Effects.SongOfTheDead2 (
  songOfTheDead2,
  SongOfTheDead2 (..),
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Window qualified as Window

newtype SongOfTheDead2 = SongOfTheDead2 EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: EffectArgs -> SongOfTheDead2
songOfTheDead2 = SongOfTheDead2 . uncurry4 (baseAttrs "02112")

instance HasModifiersFor SongOfTheDead2

instance RunMessage SongOfTheDead2 where
  runMessage msg e@(SongOfTheDead2 attrs@EffectAttrs {..}) = case msg of
    RevealChaosToken _ iid token@(ChaosToken _ Skull) | InvestigatorTarget iid == effectTarget -> do
      push $
        If
          (Window.RevealChaosTokenEffect iid token effectId)
          [skillTestModifier attrs effectTarget (DamageDealt 2)]
      pure e
    SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
    _ -> SongOfTheDead2 <$> runMessage msg attrs
