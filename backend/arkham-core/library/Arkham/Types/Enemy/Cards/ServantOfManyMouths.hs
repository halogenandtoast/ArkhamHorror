module Arkham.Types.Enemy.Cards.ServantOfManyMouths
  ( ServantOfManyMouths(..)
  , servantOfManyMouths
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


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyId -> ServantOfManyMouths
servantOfManyMouths uuid =
  ServantOfManyMouths
    $ baseAttrs uuid "02224"
    $ (healthDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 2)
    . (evadeL .~ 1)

instance HasModifiersFor env ServantOfManyMouths where
  getModifiersFor = noModifiersFor

ability :: EnemyAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance ActionRunner env => HasActions env ServantOfManyMouths where
  getActions iid (AfterEnemyDefeated You eid) (ServantOfManyMouths attrs)
    | eid == toId attrs = pure [ActivateCardAbilityAction iid (ability attrs)]
  getActions i window (ServantOfManyMouths attrs) = getActions i window attrs

instance EnemyRunner env => RunMessage env ServantOfManyMouths where
  runMessage msg e@(ServantOfManyMouths attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationIds <- getSetList ()
      locationsWithClues <- filterM
        (fmap ((> 0) . unClueCount) . getCount)
        locationIds
      e <$ unless
        (null locationsWithClues)
        (unshiftMessage
          (chooseOne
            iid
            [ TargetLabel
                (LocationTarget lid)
                [DiscoverCluesAtLocation iid lid 1 Nothing]
            | lid <- locationsWithClues
            ]
          )
        )
    _ -> ServantOfManyMouths <$> runMessage msg attrs
