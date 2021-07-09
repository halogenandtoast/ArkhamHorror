module Arkham.Types.Event.Cards.HypnoticGaze
  ( hypnoticGaze
  , HypnoticGaze(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Token

newtype HypnoticGaze = HypnoticGaze (EventAttrs `With` Maybe EnemyId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze :: EventCard HypnoticGaze
hypnoticGaze = event (HypnoticGaze . (`with` Nothing)) Cards.hypnoticGaze

instance HasActions env HypnoticGaze where
  getActions iid window (HypnoticGaze (attrs `With` _)) =
    getActions iid window attrs

instance HasModifiersFor env HypnoticGaze where
  getModifiersFor = noModifiersFor

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance HasCount HealthDamageCount env EnemyId => RunMessage env HypnoticGaze where
  runMessage msg e@(HypnoticGaze (attrs `With` mEnemyId)) = case msg of
    InvestigatorPlayEvent iid eventId _ _ | eventId == toId attrs -> do
      enemyId <- withQueue $ \queue ->
        let PerformEnemyAttack _ eid : queue' = dropUntilAttack queue
        in (queue', eid)
      push (RequestTokens (toSource attrs) (Just iid) 1 SetAside)
      pure $ HypnoticGaze (attrs `with` Just enemyId)
    RequestedTokens source (Just iid) faces | isSource attrs source -> do
      let
        enemyId = fromMaybe (error "missing enemy id") mEnemyId
        shouldDamageEnemy =
          any (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) faces
      if shouldDamageEnemy
        then do
          healthDamage' <- unHealthDamageCount <$> getCount enemyId
          e <$ pushAll
            [ EnemyDamage enemyId iid (toSource attrs) healthDamage'
            , Discard $ toTarget attrs
            ]
        else e <$ push (Discard $ toTarget attrs)
    _ -> HypnoticGaze . (`with` mEnemyId) <$> runMessage msg attrs
