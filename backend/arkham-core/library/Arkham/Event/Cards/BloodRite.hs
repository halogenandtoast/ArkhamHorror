module Arkham.Event.Cards.BloodRite
  ( bloodRite
  , BloodRite(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Card
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

instance RunMessage BloodRite where
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
      -> do
        if length discardedCards == 2
          then push (UseCardAbility iid source windows 1 payment)
          else do
            cards <- fieldMap InvestigatorHand (filter isDiscardable) iid
            push
              $ (chooseOne iid
                $ [ TargetLabel (CardIdTarget $ toCardId card)
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
        pure e
    UseCardAbility iid source _ 1 (DiscardCardPayment xs)
      | isSource attrs source -> do
        enemyIds <-
          selectList $ EnemyAt $ LocationWithInvestigator $ InvestigatorWithId
            iid
        pushAll $ replicate
          (length xs)
          (chooseOne iid
          $ [Label "Gain Resource" [TakeResources iid 1 False]]
          <> [ Label
                 "Spend Resource and Deal 1 Damage To Enemy At Your Location"
                 [ SpendResources iid 1
                 , chooseOne
                   iid
                   [ TargetLabel
                       (EnemyTarget enemyId)
                       [ EnemyDamage
                           enemyId
                           iid
                           source
                           NonAttackDamageEffect
                           1
                       ]
                   | enemyId <- enemyIds
                   ]
                 ]
             | notNull enemyIds
             ]
          )
        pure e
    _ -> BloodRite <$> runMessage msg attrs
