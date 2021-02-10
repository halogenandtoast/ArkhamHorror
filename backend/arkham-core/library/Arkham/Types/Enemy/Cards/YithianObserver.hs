module Arkham.Types.Enemy.Cards.YithianObserver where

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

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyId -> YithianObserver
yithianObserver uuid =
  YithianObserver
    $ baseAttrs uuid "01177"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ FewestCards)

instance HasModifiersFor env YithianObserver where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env YithianObserver where
  getActions i window (YithianObserver attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env YithianObserver where
  runMessage msg e@(YithianObserver attrs@EnemyAttrs {..}) = case msg of
    PerformEnemyAttack iid eid | eid == enemyId -> do
      cardCount' <- unCardCount <$> getCount iid
      if cardCount' == 0
        then e <$ unshiftMessage
          (InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            DamageAny
            (enemyHealthDamage + 1)
            (enemySanityDamage + 1)
          )
        else e <$ unshiftMessages
          [ RandomDiscard iid
          , InvestigatorAssignDamage
            iid
            (EnemySource enemyId)
            DamageAny
            enemyHealthDamage
            enemySanityDamage
          ]
    _ -> YithianObserver <$> runMessage msg attrs
