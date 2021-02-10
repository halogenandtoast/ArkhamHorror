module Arkham.Types.Effect.Effects.WillToSurvive4
  ( willToSurvive4
  , WillToSurvive4(..)
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
import Arkham.Types.Effect.Helpers

newtype WillToSurvive4 = WillToSurvive4 EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive4 :: EffectArgs -> WillToSurvive4
willToSurvive4 = WillToSurvive4 . uncurry4 (baseAttrs "01085")

instance HasModifiersFor env WillToSurvive4 where
  getModifiersFor _ target (WillToSurvive4 a@EffectAttrs {..})
    | target == effectTarget = pure
      [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env WillToSurvive4 where
  runMessage msg e@(WillToSurvive4 attrs) = case msg of
    EndTurn _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs) -- TODO: should we actually check who?
    _ -> WillToSurvive4 <$> runMessage msg attrs
