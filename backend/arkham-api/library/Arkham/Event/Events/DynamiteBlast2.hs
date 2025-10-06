module Arkham.Event.Events.DynamiteBlast2 (dynamiteBlast2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Modifier
import Arkham.Projection

newtype DynamiteBlast2 = DynamiteBlast2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast2 :: EventCard DynamiteBlast2
dynamiteBlast2 = event DynamiteBlast2 Cards.dynamiteBlast2

instance RunMessage DynamiteBlast2 where
  runMessage msg e@(DynamiteBlast2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid \current -> do
        connectedLocationIds <- select $ accessibleFrom NotForMovement current
        canDealDamage <- withoutModifier iid CannotDealDamage
        chooseOneM iid do
          for_ (current : connectedLocationIds) \lid -> do
            enemies <- if canDealDamage then select (enemyAt lid) else pure []
            mconcealed <-
              runMaybeT $ guard canDealDamage >> MaybeT (fieldMap LocationConcealedCards headMay lid)
            investigators <- select $ investigatorAt lid
            unless (null enemies && null investigators && isNothing mconcealed) do
              targeting lid do
                uiEffect attrs lid Explosion
                for_ enemies $ nonAttackEnemyDamage (Just iid) attrs 3
                for_ investigators \iid' -> assignDamage iid' attrs 3
                for_ mconcealed \concealed -> do
                  chooseOneM iid do
                    labeled "Expose concealed card" $ push $ Flip iid GameSource (ConcealedCardTarget concealed)
                    labeled "Do not expose concealed card" nothing
      pure e
    _ -> DynamiteBlast2 <$> liftRunMessage msg attrs
