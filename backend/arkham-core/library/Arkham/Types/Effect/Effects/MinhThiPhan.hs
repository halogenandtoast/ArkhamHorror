module Arkham.Types.Effect.Effects.MinhThiPhan
  ( minhThiPhan
  , MinhThiPhan(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token

newtype MinhThiPhan = MinhThiPhan EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

minhThiPhan :: EffectArgs -> MinhThiPhan
minhThiPhan = MinhThiPhan . uncurry4 (baseAttrs "03002")

instance HasModifiersFor env MinhThiPhan where
  getModifiersFor _ target@(CardIdTarget _) (MinhThiPhan attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [AddSkillIcons [SkillWild]]
  getModifiersFor _ target@(SkillTarget _) (MinhThiPhan attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [ReturnToHandAfterTest]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env MinhThiPhan where
  runMessage msg e@(MinhThiPhan attrs) = case msg of
    SkillTestEnds _ | effectSource attrs == TokenEffectSource ElderSign ->
      case effectMetadata attrs of
        Just (EffectMessages msgs) ->
          e <$ pushAll (DisableEffect (effectId attrs) : msgs)
        _ -> e <$ push (DisableEffect $ effectId attrs)
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> MinhThiPhan <$> runMessage msg attrs
