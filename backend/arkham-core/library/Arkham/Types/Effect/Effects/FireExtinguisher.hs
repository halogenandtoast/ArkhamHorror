module Arkham.Types.Effect.Effects.FireExtinguisher
  ( fireExtinguisher
  , FireExtinguisher(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target

newtype FireExtinguisher = FireExtinguisher EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireExtinguisher :: EffectArgs -> FireExtinguisher
fireExtinguisher = FireExtinguisher . uncurry4 (baseAttrs "02114")

instance HasModifiersFor env FireExtinguisher where
  getModifiersFor = noModifiersFor

instance HasSet EnemyId env InvestigatorId => RunMessage env FireExtinguisher where
  runMessage msg e@(FireExtinguisher attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget _)) _ _
      | SkillTestTarget == effectTarget
      -> do
        evasions <- map (EnemyEvaded iid) <$> getSetList @EnemyId iid
        e <$ unshiftMessages (evasions <> [DisableEffect effectId])
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> FireExtinguisher <$> runMessage msg attrs
