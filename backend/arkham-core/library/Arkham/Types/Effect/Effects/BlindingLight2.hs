module Arkham.Types.Effect.Effects.BlindingLight2
  ( blindingLight2
  , BlindingLight2(..)
  )
where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype BlindingLight2 = BlindingLight2 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight2 :: EffectArgs -> BlindingLight2
blindingLight2 = BlindingLight2 . uncurry4 (baseAttrs "01069")

instance HasModifiersFor env BlindingLight2 where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BlindingLight2 where
  runMessage msg e@(BlindingLight2 attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessages
          [ LoseActions iid (toSource attrs) 1
          , InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
          , DisableEffect effectId
          ]
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ unshiftMessages
        [EnemyDamage eid iid (InvestigatorSource iid) 2, DisableEffect effectId]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BlindingLight2 <$> runMessage msg attrs
