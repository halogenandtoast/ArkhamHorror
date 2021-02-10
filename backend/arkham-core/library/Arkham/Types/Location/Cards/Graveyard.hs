module Arkham.Types.Location.Cards.Graveyard where

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


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype Graveyard = Graveyard LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: Graveyard
graveyard = Graveyard $ base { locationVictory = Just 1 }
 where
  base = baseAttrs
    "01133"
    (Name "Graveyard" Nothing)
    EncounterSet.TheMidnightMasks
    1
    (PerPlayer 2)
    Hourglass
    [Circle]
    [Arkham]

instance HasModifiersFor env Graveyard where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Graveyard where
  getActions i window (Graveyard attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Graveyard where
  runMessage msg l@(Graveyard attrs@LocationAttrs {..}) = case msg of
    AfterEnterLocation iid lid | lid == locationId -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (LocationSource lid)
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          3
        )
      Graveyard <$> runMessage msg attrs
    FailedSkillTest iid _ source _ _ _ | isSource attrs source ->
      l <$ unshiftMessage
        (chooseOne
          iid
          [ InvestigatorAssignDamage iid source DamageAny 0 2
          , MoveTo iid "01125"
          ]
        )
    _ -> Graveyard <$> runMessage msg attrs
