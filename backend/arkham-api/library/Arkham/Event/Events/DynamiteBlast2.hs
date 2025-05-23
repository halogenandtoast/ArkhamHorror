module Arkham.Event.Events.DynamiteBlast2 (dynamiteBlast2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Modifier

newtype DynamiteBlast2 = DynamiteBlast2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast2 :: EventCard DynamiteBlast2
dynamiteBlast2 = event DynamiteBlast2 Cards.dynamiteBlast2

instance RunMessage DynamiteBlast2 where
  runMessage msg e@(DynamiteBlast2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \current -> do
        connectedLocationIds <- select $ accessibleFrom current
        canDealDamage <- withoutModifier iid CannotDealDamage
        chooseOneM iid do
          for_ (current : connectedLocationIds) \lid -> do
            enemies <- if canDealDamage then select (enemyAt lid) else pure []
            investigators <- select $ investigatorAt lid
            unless (null enemies && null investigators) do
              targeting lid do
                uiEffect attrs lid Explosion
                for_ enemies $ nonAttackEnemyDamage (Just iid) attrs 3
                for_ investigators \iid' -> assignDamage iid' attrs 3
      pure e
    _ -> DynamiteBlast2 <$> liftRunMessage msg attrs
