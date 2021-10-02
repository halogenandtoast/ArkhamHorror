module Arkham.Types.Event.Cards.DynamiteBlast where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.DamageEffect
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: EventCard DynamiteBlast
dynamiteBlast = event DynamiteBlast Cards.dynamiteBlast

instance EventRunner env => RunMessage env DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs@EventAttrs {..}) = case msg of
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
              (\enid ->
                EnemyDamage enid iid (EventSource eid) NonAttackDamageEffect 3
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
    _ -> DynamiteBlast <$> runMessage msg attrs
