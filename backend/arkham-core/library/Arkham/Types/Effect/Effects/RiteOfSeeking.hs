module Arkham.Types.Effect.Effects.RiteOfSeeking
  ( riteOfSeeking
  , RiteOfSeeking(..)
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


import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs

newtype RiteOfSeeking = RiteOfSeeking EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: EffectArgs -> RiteOfSeeking
riteOfSeeking = RiteOfSeeking . uncurry4 (baseAttrs "02028")

instance HasModifiersFor env RiteOfSeeking where
  getModifiersFor = noModifiersFor

instance (HasQueue env) => RunMessage env RiteOfSeeking where
  runMessage msg e@(RiteOfSeeking attrs@EffectAttrs {..}) = case msg of
    RevealToken _ iid token -> case effectTarget of
      InvestigationTarget iid' _ | iid == iid' -> e <$ when
        (token `elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
        (unshiftMessage $ CreateEffect
          "02028"
          Nothing
          (toSource attrs)
          (InvestigatorTarget iid)
        )
      _ -> pure e
    SkillTestEnds _ -> e <$ case effectTarget of
      InvestigatorTarget iid ->
        unshiftMessages [DisableEffect effectId, EndTurn iid]
      _ -> unshiftMessage (DisableEffect effectId)
    SuccessfulInvestigation iid _ source | isSource attrs source ->
      case effectTarget of
        InvestigationTarget _ lid' -> e <$ unshiftMessage
          (InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate))
        _ -> pure e
    _ -> RiteOfSeeking <$> runMessage msg attrs
