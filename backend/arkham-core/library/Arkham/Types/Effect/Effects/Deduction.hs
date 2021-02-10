module Arkham.Types.Effect.Effects.Deduction
  ( deduction
  , Deduction(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EffectMetadata
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype Deduction = Deduction EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction :: EffectArgs -> Deduction
deduction = Deduction . uncurry4 (baseAttrs "01039")

instance HasModifiersFor env Deduction where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env Deduction where
  runMessage msg e@(Deduction attrs@EffectAttrs {..}) = case msg of
    SuccessfulInvestigation iid lid _ -> case effectMetadata of
      Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' ->
        e <$ unshiftMessage
          (InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate))
      _ -> pure e
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> Deduction <$> runMessage msg attrs
