module Arkham.Types.Agenda.Cards.ReturnToPredatorOrPrey
  ( ReturnToPredatorOrPrey(..)
  , returnToPredatorOrPrey
  ) where

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
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype ReturnToPredatorOrPrey = ReturnToPredatorOrPrey AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToPredatorOrPrey :: ReturnToPredatorOrPrey
returnToPredatorOrPrey = ReturnToPredatorOrPrey
  $ baseAttrs "50026" "Predator or Prey?" (Agenda 1 A) (Static 6)

instance HasModifiersFor env ReturnToPredatorOrPrey where
  getModifiersFor = noModifiersFor

instance HasActions env ReturnToPredatorOrPrey where
  getActions iid NonFast (ReturnToPredatorOrPrey attrs) = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource attrs)
          1
          (ActionAbility (Just Action.Resign) (ActionCost 1))
        )
    ]
  getActions _ _ _ = pure []

instance AgendaRunner env => RunMessage env ReturnToPredatorOrPrey where
  runMessage msg a@(ReturnToPredatorOrPrey attrs@AgendaAttrs {..}) =
    case msg of
      AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B -> do
        narogath <- EncounterCard <$> genEncounterCard "50026b"
        a <$ unshiftMessages
          [CreateEnemyEngagedWithPrey narogath, NextAgenda aid "01122"]
      UseCardAbility iid (AgendaSource aid) _ 1 _ | aid == agendaId -> do
        unshiftMessage (Resign iid)
        ReturnToPredatorOrPrey <$> runMessage msg attrs
      _ -> ReturnToPredatorOrPrey <$> runMessage msg attrs
