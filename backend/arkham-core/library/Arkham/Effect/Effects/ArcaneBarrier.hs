module Arkham.Effect.Effects.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype ArcaneBarrier = ArcaneBarrier EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: EffectArgs -> ArcaneBarrier
arcaneBarrier = ArcaneBarrier . uncurry4 (baseAttrs "02102")

instance RunMessage ArcaneBarrier where
  runMessage msg e@(ArcaneBarrier attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId -> do
      push $ beginSkillTest iid (toSource attrs) (InvestigatorTarget iid) SkillWillpower 4
      pure e
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        let
          moveMessages = case effectMetadata of
            Just (EffectMessages msgs) -> msgs
            _ -> error "messages must be supplied"
        e <$ pushAll
          [ chooseOne
            iid
            [ Label "Cancel Move" []
            , Label
              "Discard top 5 cards of your deck"
              (DiscardTopOfDeck iid 5 effectSource Nothing : moveMessages)
            ]
          , DisableEffect effectId
          ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        let
          moveMessages = case effectMetadata of
            Just (EffectMessages msgs) -> msgs
            _ -> error "messages must be supplied"
        case effectSource of
          TreacherySource tid ->
            e <$ pushAll (Discard effectSource (TreacheryTarget tid) : moveMessages)
          _ -> error "Has to be a treachery source"
    _ -> ArcaneBarrier <$> runMessage msg attrs
