module Arkham.Types.Effect.Effects.BlindingLight
  ( blindingLight
  , BlindingLight(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype BlindingLight = BlindingLight EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blindingLight :: EffectArgs -> BlindingLight
blindingLight = BlindingLight . uncurry4 (baseAttrs "01066")

instance HasModifiersFor env BlindingLight where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BlindingLight where
  runMessage msg e@(BlindingLight attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessages
          [LoseActions iid (toSource attrs) 1, DisableEffect effectId]
        )
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _
      | SkillTestTarget == effectTarget
      -> e <$ unshiftMessages
        [EnemyDamage eid iid (InvestigatorSource iid) 1, DisableEffect effectId]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BlindingLight <$> runMessage msg attrs
