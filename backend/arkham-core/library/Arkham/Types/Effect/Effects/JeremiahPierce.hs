module Arkham.Types.Effect.Effects.JeremiahPierce
  ( jeremiahPierce
  , JeremiahPierce(..)
  )
where

import Arkham.Import

import Arkham.Types.Action
import Arkham.Types.Effect.Attrs

newtype JeremiahPierce = JeremiahPierce Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jeremiahPierce :: Source -> Target -> EffectId -> JeremiahPierce
jeremiahPierce source target eid =
  JeremiahPierce $ baseAttrs eid "50044" source target

instance HasModifiersFor env JeremiahPierce where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs) = case msg of
    CreatedEffect _ (InvestigatorTarget iid) eid | eid == effectId attrs ->
      e <$ unshiftMessage
        (BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          (Just Parley)
          SkillWillpower
          4
          mempty
          mempty
          mempty
          mempty
        )
    FailedSkillTest _ _ source SkillTestInitiatorTarget n
      | isSource attrs source -> e <$ unshiftMessages
        (replicate n PlaceDoomOnAgenda <> [AdvanceAgendaIfThresholdSatisfied])
    _ -> JeremiahPierce <$> runMessage msg attrs
