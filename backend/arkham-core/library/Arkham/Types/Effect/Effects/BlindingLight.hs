module Arkham.Types.Effect.Effects.BlindingLight
  ( blindingLight
  , BlindingLight(..)
  )
where

import Arkham.Import

import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype BlindingLight = BlindingLight Attrs
  deriving newtype (Show, ToJSON, FromJSON)

blindingLight :: EffectArgs -> BlindingLight
blindingLight = BlindingLight . uncurry4 (baseAttrs "01066")

instance HasModifiersFor env BlindingLight where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BlindingLight where
  runMessage msg e@(BlindingLight attrs@Attrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessages
          [LoseActions iid (toSource attrs) 1, DisableEffect effectId]
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _
      | SkillTestTarget == effectTarget
      -> e <$ unshiftMessages
        [EnemyDamage eid iid (InvestigatorSource iid) 1, DisableEffect effectId]
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BlindingLight <$> runMessage msg attrs
