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
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "must be at a location")
          iid
      connectedLocationIds <-
        selectList
          $ AccessibleFrom
          $ LocationWithId
            currentLocationId
      canDealDamage <- withoutModifier iid CannotDealDamage
      choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- if canDealDamage then selectList (enemyAt lid) else pure []
        investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
        pure
          ( lid
          , map (\enid -> EnemyDamage enid $ nonAttack attrs 3) enemyIds
              <> map
                ( \iid' ->
                    InvestigatorAssignDamage
                      iid'
                      (EventSource eid)
                      DamageAny
                      3
                      0
                )
                investigatorIds
          )
      let
        availableChoices =
          map (\(l, c) -> targetLabel l c) $ filter (notNull . snd) choices
      e <$ pushAll [chooseOne iid availableChoices]
    _ -> DynamiteBlast2 <$> runMessage msg attrs
