module Arkham.Types.Enemy.Cards.BillyCooper where

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
import Arkham.Types.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billyCooper :: EnemyId -> BillyCooper
billyCooper uuid =
  BillyCooper
    $ baseAttrs uuid "50045"
    $ (healthDamageL .~ 2)
    . (fightL .~ 5)
    . (healthL .~ Static 4)
    . (evadeL .~ 2)
    . (uniqueL .~ True)

instance HasModifiersFor env BillyCooper where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env BillyCooper where
  getActions iid window (BillyCooper attrs) = getActions iid window attrs

instance (EnemyRunner env) => RunMessage env BillyCooper where
  runMessage msg e@(BillyCooper attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Easttown")
    After (EnemyDefeated _ _ lid _ _ traits)
      | lid == enemyLocation && Monster `elem` traits -> e
      <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> BillyCooper <$> runMessage msg attrs
