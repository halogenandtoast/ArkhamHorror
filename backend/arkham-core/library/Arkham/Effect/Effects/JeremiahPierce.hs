module Arkham.Effect.Effects.JeremiahPierce (
  jeremiahPierce,
  JeremiahPierce (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.SkillType

newtype JeremiahPierce = JeremiahPierce EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeremiahPierce :: EffectArgs -> JeremiahPierce
jeremiahPierce = JeremiahPierce . uncurry4 (baseAttrs "50044")

instance RunMessage JeremiahPierce where
  runMessage msg e@(JeremiahPierce attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      pushAll
        [ parley
            iid
            (toSource attrs)
            (InvestigatorTarget iid)
            SkillWillpower
            4
        , DisableEffect $ effectId attrs
        ]
      pure e
    FailedSkillTest _ _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source ->
          e
            <$ pushAll
              (replicate n PlaceDoomOnAgenda <> [AdvanceAgendaIfThresholdSatisfied])
    _ -> JeremiahPierce <$> runMessage msg attrs
