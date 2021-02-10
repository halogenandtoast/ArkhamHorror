module Arkham.Types.Treachery.Cards.ClawsOfSteam
  ( clawsOfSteam
  , ClawsOfSteam(..)
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


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ClawsOfSteam = ClawsOfSteam TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clawsOfSteam :: TreacheryId -> a -> ClawsOfSteam
clawsOfSteam uuid _ = ClawsOfSteam $ baseAttrs uuid "02180"

instance HasModifiersFor env ClawsOfSteam where
  getModifiersFor = noModifiersFor

instance HasActions env ClawsOfSteam where
  getActions i window (ClawsOfSteam attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessages
      [ RevelationSkillTest iid source SkillWillpower 3
      , Discard (toTarget attrs)
      ]
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [CannotMove])
          source
          (InvestigatorTarget iid)
        , InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAssetsFirst
          2
          0
        ]
    _ -> ClawsOfSteam <$> runMessage msg attrs
