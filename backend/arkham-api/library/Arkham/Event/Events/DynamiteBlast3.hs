module Arkham.Event.Events.DynamiteBlast3 (dynamiteBlast3) where

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.ForMovement
import Arkham.Helpers.Modifiers
import Arkham.Helpers.UI
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Prelude
import Arkham.Projection

newtype DynamiteBlast3 = DynamiteBlast3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast3 :: EventCard DynamiteBlast3
dynamiteBlast3 = event DynamiteBlast3 Cards.dynamiteBlast3

instance RunMessage DynamiteBlast3 where
  runMessage msg e@(DynamiteBlast3 attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      currentLocationId <- fieldJust InvestigatorLocation iid
      connectedLocationIds <- select $ AccessibleFrom NotForMovement $ LocationWithId currentLocationId
      canDealDamage <- withoutModifier iid CannotDealDamage
      choices <- forMaybeM (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- if canDealDamage then select (enemyAt lid) else pure []
        investigatorIds <- select $ investigatorAt lid
        if null enemyIds && null investigatorIds
          then pure Nothing
          else do
            animation <- uiEffect eid lid Explosion
            pure
              $ Just
                ( lid
                , animation
                    : map (\enid -> EnemyDamage enid $ nonAttack (Just iid) attrs 3) enemyIds
                      <> map (\iid' -> assignDamage iid' eid 3) investigatorIds
                )
      let availableChoices = map (uncurry targetLabel) $ filter (notNull . snd) choices
      player <- getPlayer iid
      pushAll [chooseOne player availableChoices]
      pure e
    _ -> DynamiteBlast3 <$> runMessage msg attrs
