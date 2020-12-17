module Arkham.Types.Effect.Effects.Deduction
  ( deduction
  , Deduction(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype Deduction = Deduction Attrs
  deriving newtype (Show, ToJSON, FromJSON)

deduction :: EffectArgs -> Deduction
deduction = Deduction . uncurry4 (baseAttrs "01039")

instance HasModifiersFor env Deduction where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env Deduction where
  runMessage msg e@(Deduction attrs@Attrs {..}) = case msg of
    SuccessfulInvestigation iid lid _ -> case effectMetadata of
      Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' ->
        e <$ unshiftMessage (InvestigatorDiscoverClues iid lid 1)
      _ -> pure e
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> Deduction <$> runMessage msg attrs
