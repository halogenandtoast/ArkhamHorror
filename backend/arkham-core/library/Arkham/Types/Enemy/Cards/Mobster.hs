module Arkham.Types.Enemy.Cards.Mobster where

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
import Arkham.Types.Enemy.Runner

newtype Mobster = Mobster EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobster :: EnemyId -> Mobster
mobster uuid =
  Mobster
    $ baseAttrs uuid "02098"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 0)
    . (fightL .~ 2)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)

instance HasModifiersFor env Mobster where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Mobster where
  getActions i window (Mobster attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env Mobster where
  runMessage msg e@(Mobster attrs@EnemyAttrs {..}) = case msg of
    After (PerformEnemyAttack iid eid) | eid == enemyId ->
      e <$ unshiftMessage (SpendResources iid 1)
    _ -> Mobster <$> runMessage msg attrs
