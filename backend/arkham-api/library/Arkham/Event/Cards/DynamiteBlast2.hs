module Arkham.Event.Cards.DynamiteBlast2 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Projection

newtype DynamiteBlast2 = DynamiteBlast2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast2 :: EventCard DynamiteBlast2
dynamiteBlast2 = event DynamiteBlast2 Cards.dynamiteBlast2

instance RunMessage DynamiteBlast2 where
  -- TODO: Does not provoke attacks of opportunity
  runMessage msg e@(DynamiteBlast2 attrs@EventAttrs {..}) = case msg of
    PlayThisEvent iid eid | eid == eventId -> do
      currentLocationId <- fieldJust InvestigatorLocation iid
      connectedLocationIds <- select $ AccessibleFrom $ LocationWithId currentLocationId
      canDealDamage <- withoutModifier iid CannotDealDamage
      choices <- forMaybeM (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- if canDealDamage then select (enemyAt lid) else pure []
        investigatorIds <- select $ investigatorAt lid
        if null enemyIds && null investigatorIds
          then pure Nothing
          else
            pure
              $ Just
                ( lid
                , uiEffect attrs lid Explosion
                    : map (\enid -> EnemyDamage enid $ nonAttack attrs 3) enemyIds
                      <> map (\iid' -> assignDamage iid' eid 3) investigatorIds
                )
      let availableChoices = map (\(l, c) -> targetLabel l c) $ filter (notNull . snd) choices
      player <- getPlayer iid
      pushAll [chooseOne player availableChoices]
      pure e
    _ -> DynamiteBlast2 <$> runMessage msg attrs
