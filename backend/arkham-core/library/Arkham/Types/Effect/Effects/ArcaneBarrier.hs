module Arkham.Types.Effect.Effects.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype ArcaneBarrier = ArcaneBarrier EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: EffectArgs -> ArcaneBarrier
arcaneBarrier = ArcaneBarrier . uncurry4 (baseAttrs "02102")

instance HasModifiersFor env ArcaneBarrier

instance HasQueue env => RunMessage env ArcaneBarrier where
  runMessage msg e@(ArcaneBarrier attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId ->
      e <$ push
        (BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          4
        )
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
              (DiscardTopOfDeck iid 5 Nothing : moveMessages)
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
            e <$ pushAll (Discard (TreacheryTarget tid) : moveMessages)
          _ -> error "Has to be a treachery source"
    _ -> ArcaneBarrier <$> runMessage msg attrs
