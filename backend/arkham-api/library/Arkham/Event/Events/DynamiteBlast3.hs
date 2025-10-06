module Arkham.Event.Events.DynamiteBlast3 (dynamiteBlast3) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Projection
import Arkham.UI

newtype DynamiteBlast3 = DynamiteBlast3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast3 :: EventCard DynamiteBlast3
dynamiteBlast3 = event DynamiteBlast3 Cards.dynamiteBlast3

instance RunMessage DynamiteBlast3 where
  runMessage msg e@(DynamiteBlast3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      currentLocationId <- fieldJust InvestigatorLocation iid
      connectedLocationIds <- select $ AccessibleFrom NotForMovement $ LocationWithId currentLocationId
      canDealDamage <- withoutModifier iid CannotDealDamage
      chooseOneM iid do
        for_ (currentLocationId : connectedLocationIds) \lid -> do
          enemies <- if canDealDamage then select (enemyAt lid) else pure []
          mconcealed <-
            runMaybeT $ guard canDealDamage >> MaybeT (fieldMap LocationConcealedCards headMay lid)
          investigators <- select $ investigatorAt lid
          unless (null enemies && null investigators && isNothing mconcealed) do
            targeting lid do
              uiEffect attrs lid Explosion
              for_ enemies (nonAttackEnemyDamage (Just iid) attrs 3)
              for_ investigators \iid' -> assignDamage iid' attrs 3
              for_ mconcealed \concealed -> do
                chooseOneM iid do
                  labeled "Expose concealed card" $ push $ Flip iid GameSource (ConcealedCardTarget concealed)
                  labeled "Do not expose concealed card" nothing
      pure e
    _ -> DynamiteBlast3 <$> liftRunMessage msg attrs
