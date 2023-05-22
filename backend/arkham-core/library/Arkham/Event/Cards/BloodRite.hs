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
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Projection

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

instance RunMessage BloodRite where
  runMessage msg e@(BloodRite attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == eventId -> do
      drawing <- drawCards iid attrs 2
      pushAll
        [ drawing
        , PayForCardAbility
          iid
          (EventSource eid)
          windows
          1
          (DiscardCardPayment [])
        ]
      pure e
    PayForCardAbility iid source windows 1 payment@(DiscardCardPayment discardedCards)
      | isSource attrs source
      -> do
        if length discardedCards == 2
          then push (UseCardAbility iid source 1 windows payment)
          else do
            cards <- fieldMap InvestigatorHand (filter isDiscardable) iid
            push
              $ (chooseOne iid
                $ [ TargetLabel
                      (CardIdTarget $ toCardId card)
                      [ DiscardCard iid (toSource attrs) (toCardId card)
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
                       [UseCardAbility iid source 1 windows payment]
                   ]
                )
        pure e
    UseCardAbility iid source 1 _ (DiscardCardPayment xs)
      | isSource attrs source -> do
        enemyIds <-
          selectList $ EnemyAt $ LocationWithInvestigator $ InvestigatorWithId
            iid
        pushAll $ replicate
          (length xs)
          (chooseOne iid
          $ [Label "Gain Resource" [TakeResources iid 1 (toAbilitySource attrs 1) False]]
          <> [ Label
                 "Spend Resource and Deal 1 Damage To Enemy At Your Location"
                 [ SpendResources iid 1
                 , chooseOne
                   iid
                   [ targetLabel
                       enemyId
                       [EnemyDamage enemyId $ nonAttack source 1]
                   | enemyId <- enemyIds
                   ]
                 ]
             | notNull enemyIds
             ]
          )
        pure e
    _ -> BloodRite <$> runMessage msg attrs
