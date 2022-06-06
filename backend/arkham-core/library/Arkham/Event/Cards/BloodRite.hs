module Arkham.Event.Cards.BloodRite
  ( bloodRite
  , BloodRite(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Source

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

instance EventRunner env => RunMessage BloodRite where
  runMessage msg e@(BloodRite attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == eventId -> e <$ pushAll
      [ DrawCards iid 2 False
      , PayForCardAbility
        iid
        (EventSource eid)
        windows
        1
        (DiscardCardPayment [])
      , Discard (toTarget attrs)
      ]
    PayForCardAbility iid source windows 1 payment@(DiscardCardPayment discardedCards)
      | isSource attrs source
      -> if length discardedCards == 2
        then e <$ push (UseCardAbility iid source windows 1 payment)
        else do
          cards <- map unDiscardableHandCard <$> getList iid
          e <$ push
            (chooseOne iid
            $ [ Run
                  [ DiscardCard iid (toCardId card)
                  , PayForCardAbility
                    iid
                    source
                    windows
                    1
                    (DiscardCardPayment $ card : discardedCards)
                  ]
              | card <- cards
              ]
            <> [ Label
                   ("Continue having discarded "
                   <> tshow (length discardedCards)
                   <> " cards"
                   )
                   [UseCardAbility iid source windows 1 payment]
               ]
            )
    UseCardAbility iid source _ 1 (DiscardCardPayment xs)
      | isSource attrs source -> do
        locationId <- getId @LocationId iid
        enemyIds <- getSetList @EnemyId locationId
        e <$ pushAll
          (replicate
            (length xs)
            (chooseOne iid
            $ [Label "Gain Resource" [TakeResources iid 1 False]]
            <> [ Label
                   "Spend Resource and Deal 1 Damage To Enemy At Your Location"
                   [ SpendResources iid 1
                   , chooseOne
                     iid
                     [ EnemyDamage enemyId iid source NonAttackDamageEffect 1
                     | enemyId <- enemyIds
                     ]
                   ]
               | notNull enemyIds
               ]
            )
          )
    _ -> BloodRite <$> runMessage msg attrs
