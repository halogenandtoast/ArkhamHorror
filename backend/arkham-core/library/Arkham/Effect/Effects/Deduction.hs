module Arkham.Effect.Effects.Deduction
  ( deduction
  , Deduction(..)
  ) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.EffectMetadata
import Arkham.Message
import Arkham.Target

newtype Deduction = Deduction EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: EffectArgs -> Deduction
deduction = Deduction . uncurry4 (baseAttrs "01039")

instance HasModifiersFor env Deduction

instance HasQueue env => RunMessage env Deduction where
  runMessage msg e@(Deduction attrs@EffectAttrs {..}) = case msg of
    Successful (Action.Investigate, _) iid _ (LocationTarget lid) _ ->
      case effectMetadata of
        Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' ->
          e <$ push
            (InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate))
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> Deduction <$> runMessage msg attrs
