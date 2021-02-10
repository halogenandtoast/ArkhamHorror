module Arkham.Types.Effect.Effects.JeremiahPierce
  ( jeremiahPierce
  , JeremiahPierce(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Action
import Arkham.Types.Effect.Attrs

newtype JeremiahPierce = JeremiahPierce EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EffectArgs -> JeremiahPierce
jeremiahPierce = JeremiahPierce . uncurry4 (baseAttrs "50044")

instance HasModifiersFor env JeremiahPierce where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs ->
      e <$ unshiftMessages
        [ BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          (Just Parley)
          SkillWillpower
          4
        , DisableEffect $ effectId attrs
        ]
    FailedSkillTest _ _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> e <$ unshiftMessages
        (replicate n PlaceDoomOnAgenda <> [AdvanceAgendaIfThresholdSatisfied])
    _ -> JeremiahPierce <$> runMessage msg attrs
