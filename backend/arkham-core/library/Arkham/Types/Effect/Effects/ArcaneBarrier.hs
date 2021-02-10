module Arkham.Types.Effect.Effects.ArcaneBarrier
  ( ArcaneBarrier(..)
  , arcaneBarrier
  )
where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Effect.Attrs

newtype ArcaneBarrier = ArcaneBarrier EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: EffectArgs -> ArcaneBarrier
arcaneBarrier = ArcaneBarrier . uncurry4 (baseAttrs "02102")

instance HasModifiersFor env ArcaneBarrier where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env ArcaneBarrier where
  runMessage msg e@(ArcaneBarrier attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId ->
      e <$ unshiftMessage
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
        e <$ unshiftMessages
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
            e <$ unshiftMessages (Discard (TreacheryTarget tid) : moveMessages)
          _ -> error "Has to be a treachery source"
    _ -> ArcaneBarrier <$> runMessage msg attrs
