module Arkham.Types.Effect.Effects.Deduction
  ( deduction
  , Deduction(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Target

newtype Deduction = Deduction EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: EffectArgs -> Deduction
deduction = Deduction . uncurry4 (baseAttrs "01039")

instance HasModifiersFor env Deduction

instance HasQueue env => RunMessage env Deduction where
  runMessage msg e@(Deduction attrs@EffectAttrs {..}) = case msg of
    SuccessfulInvestigation iid lid _ _ -> case effectMetadata of
      Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' ->
        e <$ push
          (InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate))
      _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> Deduction <$> runMessage msg attrs
