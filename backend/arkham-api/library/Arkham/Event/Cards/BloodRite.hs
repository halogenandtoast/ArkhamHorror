module Arkham.Event.Cards.BloodRite (bloodRite, BloodRite (..)) where

import Arkham.Classes.HasGame
import Arkham.Cost hiding (discardedCards)
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype BloodRite = BloodRite EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodRite :: EventCard BloodRite
bloodRite = event BloodRite Cards.bloodRite

bloodRiteLimit :: HasGame m => EventAttrs -> m Int
bloodRiteLimit attrs = getMetaMaybe 2 (bothTarget attrs.cardId attrs) "use3"

instance RunMessage BloodRite where
  runMessage msg e@(BloodRite attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid (is attrs -> True) _ windows _ -> do
      drawCardsIfCan iid attrs =<< bloodRiteLimit attrs
      push $ PayForCardAbility iid (toSource attrs) windows 1 (DiscardCardPayment [])
      pure e
    PayForCardAbility iid (isSource attrs -> True) windows 1 payment@(DiscardCardPayment discardedCards) -> do
      limit <- bloodRiteLimit attrs
      if length discardedCards == limit
        then push $ UseCardAbility iid (toSource attrs) 1 windows payment
        else do
          cards <- fieldMap InvestigatorHand (filter isDiscardable) iid
          chooseOne iid
            $ [ targetLabel
                card
                [ DiscardCard iid (toSource attrs) card.id
                , PayForCardAbility iid (toSource attrs) windows 1 (DiscardCardPayment $ card : discardedCards)
                ]
              | card <- cards
              ]
            <> [ Label
                  ("Continue having discarded " <> tshow (length discardedCards) <> " cards")
                  [UseCardAbility iid (toSource attrs) 1 windows payment]
               ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 _ (DiscardCardPayment xs) -> do
      enemyIds <- select $ enemyAtLocationWith iid
      canDealDamage <- withoutModifier iid CannotDealDamage
      player <- getPlayer iid
      pushAll
        $ replicate (length xs)
        $ Msg.chooseOne player
        $ [Label "Gain Resource" [TakeResources iid 1 (attrs.ability 1) False]]
        <> [ Label "Spend Resource and Deal 1 Damage To Enemy At Your Location"
            $ [ SpendResources iid 1
              , Msg.chooseOne
                  player
                  [targetLabel enemy [EnemyDamage enemy $ nonAttack (attrs.ability 1) 1] | enemy <- enemyIds]
              ]
           | canDealDamage
           , notNull enemyIds
           ]
      pure e
    _ -> BloodRite <$> liftRunMessage msg attrs
