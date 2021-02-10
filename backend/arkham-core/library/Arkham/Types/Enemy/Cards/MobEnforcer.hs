module Arkham.Types.Enemy.Cards.MobEnforcer
  ( MobEnforcer(..)
  , mobEnforcer
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


import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype MobEnforcer = MobEnforcer EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mobEnforcer :: EnemyId -> MobEnforcer
mobEnforcer uuid = MobEnforcer $ (weaknessBaseAttrs uuid "01101")
  { enemyHealthDamage = 1
  , enemySanityDamage = 0
  , enemyFight = 4
  , enemyHealth = Static 3
  , enemyEvade = 3
  , enemyPrey = SetToBearer
  }

instance HasModifiersFor env MobEnforcer where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env MobEnforcer where
  getActions iid NonFast (MobEnforcer attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      resourceCount <- getResourceCount iid
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility
                (Just Parley)
                (Costs [ActionCost 1, ResourceCost 4])
              )
            )
        | resourceCount >= 4 && locationId == enemyLocation
        ]
  getActions i window (MobEnforcer attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env MobEnforcer where
  runMessage msg e@(MobEnforcer attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ unshiftMessage (Discard $ toTarget attrs)
    _ -> MobEnforcer <$> runMessage msg attrs
