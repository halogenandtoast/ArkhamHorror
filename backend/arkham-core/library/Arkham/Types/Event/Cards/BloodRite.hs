module Arkham.Types.Event.Cards.BloodRite where

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


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype BloodRite = BloodRite EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: InvestigatorId -> EventId -> BloodRite
bloodRite iid uuid = BloodRite $ baseAttrs iid uuid "05317"

instance HasModifiersFor env BloodRite where
  getModifiersFor = noModifiersFor

instance HasActions env BloodRite where
  getActions i window (BloodRite attrs) = getActions i window attrs

instance EventRunner env => RunMessage env BloodRite where
  runMessage msg e@(BloodRite attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> e <$ unshiftMessages
      [ DrawCards iid 2 False
      , PayForCardAbility iid (EventSource eid) (Just $ IntMetadata 0) 1
      , Discard (toTarget attrs)
      ]
    PayForCardAbility iid source meta@(Just (IntMetadata n)) 1
      | isSource attrs source -> if n == 2
        then e <$ unshiftMessage (UseCardAbility iid source meta 1 NoPayment)
        else do
          cards <- map unDiscardableHandCard <$> getList iid
          e <$ unshiftMessage
            (chooseOne iid
            $ [ Run
                  [ DiscardCard iid (getCardId card)
                  , PayForCardAbility
                    iid
                    source
                    (Just (IntMetadata $ n + 1))
                    1
                  ]
              | card <- cards
              ]
            <> [ Label
                   ("Continue having discarded " <> tshow n <> " cards")
                   [UseCardAbility iid source meta 1 NoPayment]
               ]
            )
    UseCardAbility iid source (Just (IntMetadata n)) 1 _
      | isSource attrs source -> do
        locationId <- getId @LocationId iid
        enemyIds <- getSetList @EnemyId locationId
        e <$ unshiftMessages
          (replicate
            n
            (chooseOne iid
            $ [Label "Gain Resource" [TakeResources iid 1 False]]
            <> [ Label
                   "Spend Resource and Deal 1 Damage To Enemy At Your Location"
                   [ SpendResources iid 1
                   , chooseOne
                     iid
                     [ EnemyDamage enemyId iid source 1
                     | enemyId <- enemyIds
                     ]
                   ]
               | not (null enemyIds)
               ]
            )
          )
    _ -> BloodRite <$> runMessage msg attrs
