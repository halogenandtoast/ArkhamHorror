module Arkham.Effect.Effects.MinhThiPhan
  ( minhThiPhan
  , MinhThiPhan(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token

newtype MinhThiPhan = MinhThiPhan EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

minhThiPhan :: EffectArgs -> MinhThiPhan
minhThiPhan = MinhThiPhan . uncurry4 (baseAttrs "03002")

instance HasModifiersFor MinhThiPhan where
  getModifiersFor target@(CardTarget _) (MinhThiPhan attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [AddSkillIcons [SkillWild]]
  getModifiersFor target@(SkillTarget _) (MinhThiPhan attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [ReturnToHandAfterTest]
  getModifiersFor _ _ = pure []

instance RunMessage MinhThiPhan where
  runMessage msg e@(MinhThiPhan attrs) = case msg of
    SkillTestEnds _ | effectSource attrs == TokenEffectSource ElderSign ->
      case effectMetadata attrs of
        Just (EffectMessages msgs) ->
          e <$ pushAll (DisableEffect (effectId attrs) : msgs)
        _ -> e <$ push (DisableEffect $ effectId attrs)
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MinhThiPhan <$> runMessage msg attrs
