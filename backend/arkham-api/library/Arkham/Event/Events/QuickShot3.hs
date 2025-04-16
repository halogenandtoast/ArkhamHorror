module Arkham.Event.Events.QuickShot3 (quickShot3) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype QuickShot3 = QuickShot3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickShot3 :: EventCard QuickShot3
quickShot3 = event QuickShot3 Cards.quickShot3

instance HasAbilities QuickShot3 where
  getAbilities (QuickShot3 x) =
    [ restrictedAbility
        x
        1
        (InYourHand <> exists (PlayableCardWithCostReduction NoAction 2 (basic $ CardWithId x.cardId)))
        $ freeReaction
        $ DrawCard #after You (basic $ CardWithId x.cardId) AnyDeck
    ]

instance RunMessage QuickShot3 where
  runMessage msg e@(QuickShot3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
      chooseTargetM iid enemies (nonAttackEnemyDamage (Just iid) attrs 1)
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      reduceCostOf (attrs.ability 1) attrs 2
      playCardPayingCost iid (toCard attrs)
      pure e
    _ -> QuickShot3 <$> liftRunMessage msg attrs
