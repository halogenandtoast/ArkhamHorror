module Arkham.Types.Effect.Effects.Burglary
  ( burglary
  , Burglary(..)
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

newtype Burglary = Burglary EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burglary :: EffectArgs -> Burglary
burglary = Burglary . uncurry4 (baseAttrs "01045")

instance HasModifiersFor env Burglary where
  getModifiersFor _ (LocationTarget lid) (Burglary attrs@EffectAttrs {..}) =
    case effectTarget of
      InvestigationTarget _ lid' | lid == lid' ->
        pure [toModifier attrs AlternateSuccessfullInvestigation]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Burglary where
  runMessage msg e@(Burglary attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigationTarget iid lid) | eid == effectId ->
      e <$ unshiftMessage
        (Investigate iid lid (toSource attrs) SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source ->
      e <$ unshiftMessages [TakeResources iid 3 False, DisableEffect effectId]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> Burglary <$> runMessage msg attrs
