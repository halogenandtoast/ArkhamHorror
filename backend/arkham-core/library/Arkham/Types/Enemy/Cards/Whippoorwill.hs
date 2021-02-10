module Arkham.Types.Enemy.Cards.Whippoorwill
  ( Whippoorwill(..)
  , whippoorwill
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


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype Whippoorwill = Whippoorwill EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whippoorwill :: EnemyId -> Whippoorwill
whippoorwill uuid =
  Whippoorwill
    $ baseAttrs uuid "02090"
    $ (sanityDamageL .~ 1)
    . (fightL .~ 1)
    . (healthL .~ Static 1)
    . (evadeL .~ 4)

instance HasId LocationId env InvestigatorId => HasModifiersFor env Whippoorwill where
  getModifiersFor _ (InvestigatorTarget iid) (Whippoorwill attrs) = do
    locationId <- getId iid
    pure $ toModifiers
      attrs
      [ AnySkillValue (-1) | locationId == enemyLocation attrs ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env Whippoorwill where
  getActions i window (Whippoorwill attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Whippoorwill where
  runMessage msg (Whippoorwill attrs) = Whippoorwill <$> runMessage msg attrs
