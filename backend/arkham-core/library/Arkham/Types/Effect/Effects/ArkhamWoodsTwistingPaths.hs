module Arkham.Types.Effect.Effects.ArkhamWoodsTwistingPaths
  ( arkhamWoodsTwistingPaths
  , ArkhamWoodsTwistingPaths(..)
  )
where


import Arkham.Types.Effect.Attrs

newtype ArkhamWoodsTwistingPaths = ArkhamWoodsTwistingPaths EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsTwistingPaths :: EffectArgs -> ArkhamWoodsTwistingPaths
arkhamWoodsTwistingPaths =
  ArkhamWoodsTwistingPaths . uncurry4 (baseAttrs "01151")

instance HasModifiersFor env ArkhamWoodsTwistingPaths where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env ArkhamWoodsTwistingPaths where
  runMessage msg e@(ArkhamWoodsTwistingPaths attrs) = case msg of
    PassedSkillTest _ _ (LocationSource "01151") SkillTestInitiatorTarget{} _ _
      -> do
        let disable = DisableEffect (effectId attrs)
        e <$ case effectMetadata attrs of
          Just (EffectMessages msgs) -> unshiftMessages (msgs <> [disable])
          _ -> unshiftMessage disable
    FailedSkillTest _ _ (LocationSource "01151") SkillTestInitiatorTarget{} _ _
      -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> ArkhamWoodsTwistingPaths <$> runMessage msg attrs
