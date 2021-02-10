module Arkham.Types.Enemy.Cards.AlmaHill
  ( AlmaHill(..)
  , almaHill
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


import Arkham.Types.Action
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyId -> AlmaHill
almaHill uuid =
  AlmaHill
    $ baseAttrs uuid "50046"
    $ (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (uniqueL .~ True)

instance HasModifiersFor env AlmaHill where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AlmaHill where
  getActions iid NonFast (AlmaHill attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (EnemySource enemyId)
              1
              (ActionAbility (Just Parley) (ActionCost 1))
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Southside")
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ unshiftMessages
        (replicate 3 (InvestigatorDrawEncounterCard iid)
        <> [AddToVictory (toTarget attrs)]
        )
    _ -> AlmaHill <$> runMessage msg attrs
