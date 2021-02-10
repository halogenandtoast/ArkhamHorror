module Arkham.Types.Skill.Cards.TrueUnderstanding
  ( trueUnderstanding
  , TrueUnderstanding(..)
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


import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype TrueUnderstanding = TrueUnderstanding SkillAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueUnderstanding :: InvestigatorId -> SkillId -> TrueUnderstanding
trueUnderstanding iid uuid = TrueUnderstanding $ baseAttrs iid uuid "04153"

instance HasModifiersFor env TrueUnderstanding where
  getModifiersFor = noModifiersFor

instance HasActions env TrueUnderstanding where
  getActions iid window (TrueUnderstanding attrs) = getActions iid window attrs

-- Investigation is not an ability on the card so we need to pass
-- Nothing for the action type

instance SkillRunner env => RunMessage env TrueUnderstanding where
  runMessage msg s@(TrueUnderstanding attrs@SkillAttrs {..}) = case msg of
    PassedSkillTest iid _ _ (SkillTarget sid) _ _ | sid == skillId -> do
      lid <- getId iid
      s <$ unshiftMessage (InvestigatorDiscoverClues iid lid 1 Nothing)
    _ -> TrueUnderstanding <$> runMessage msg attrs
