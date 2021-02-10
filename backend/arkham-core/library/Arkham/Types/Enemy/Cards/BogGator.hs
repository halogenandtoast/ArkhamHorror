module Arkham.Types.Enemy.Cards.BogGator where

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
import Arkham.Types.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bogGator :: EnemyId -> BogGator
bogGator uuid =
  BogGator
    $ baseAttrs uuid "81022"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 2)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)
    . (preyL .~ LowestSkill SkillAgility)

instance HasSet Trait env LocationId => HasModifiersFor env BogGator where
  getModifiersFor _ (EnemyTarget eid) (BogGator a@EnemyAttrs {..})
    | spawned a && eid == enemyId = do
      bayouLocation <- member Bayou <$> getSet enemyLocation
      pure $ toModifiers a $ if bayouLocation
        then [EnemyFight 2, EnemyEvade 2]
        else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env BogGator where
  getActions i window (BogGator attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
