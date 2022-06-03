module Arkham.Event.Cards.DynamiteBlast2 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Message
import Arkham.Source
import Arkham.Target

newtype DynamiteBlast2 = DynamiteBlast2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast2 :: EventCard DynamiteBlast2
dynamiteBlast2 = event DynamiteBlast2 Cards.dynamiteBlast2

instance EventRunner env => RunMessage DynamiteBlast2 where
  -- TODO: Does not provoke attacks of opportunity
  runMessage msg e@(DynamiteBlast2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      currentLocationId <- getId @LocationId iid
      connectedLocationIds <- map unConnectedLocationId
        <$> getSetList currentLocationId
      choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
        enemyIds <- getSetList lid
        investigatorIds <- getSetList @InvestigatorId lid
        pure
          ( lid
          , map
              (\enid -> EnemyDamage enid iid (EventSource eid) NonAttackDamageEffect 3
              )
              enemyIds
            <> map
                 (\iid' -> InvestigatorAssignDamage
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
          map (\(l, c) -> TargetLabel (LocationTarget l) c)
            $ filter (notNull . snd) choices
      e <$ pushAll [chooseOne iid availableChoices, Discard (EventTarget eid)]
    _ -> DynamiteBlast2 <$> runMessage msg attrs
