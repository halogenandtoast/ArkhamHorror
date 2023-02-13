module Arkham.Event.Cards.DynamiteBlast where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Projection

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: EventCard DynamiteBlast
dynamiteBlast = event DynamiteBlast Cards.dynamiteBlast

instance RunMessage DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      connectedLocationIds <- selectList $ AccessibleFrom $ LocationWithId
        currentLocationId
      choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- selectList $ EnemyAt $ LocationWithId lid
        investigatorIds <- selectList $ InvestigatorAt $ LocationWithId lid
        pure
          ( lid
          , map (\enid -> EnemyDamage enid $ nonAttack attrs 3) enemyIds
            <> map
                 (\iid' -> InvestigatorAssignDamage
                   iid'
                   (toSource attrs)
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
    _ -> DynamiteBlast <$> runMessage msg attrs
