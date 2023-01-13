module Arkham.Event.Cards.HypnoticGaze
  ( hypnoticGaze
  , HypnoticGaze(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Types hiding ( EnemyDamage )
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Projection
import Arkham.RequestedTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype HypnoticGaze = HypnoticGaze (EventAttrs `With` Maybe EnemyId)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze :: EventCard HypnoticGaze
hypnoticGaze = event (HypnoticGaze . (`with` Nothing)) Cards.hypnoticGaze

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance RunMessage HypnoticGaze where
  runMessage msg e@(HypnoticGaze (attrs `With` mEnemyId)) = case msg of
    InvestigatorPlayEvent iid eventId _ _ _ | eventId == toId attrs -> do
      enemyId <- withQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack _ eid _ _ : queue' -> (queue', eid)
        _ -> error "unhandled"
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll [RequestTokens (toSource attrs) (Just iid) (Reveal 1) SetAside, ignoreWindow, Discard (toTarget attrs)]
      pure $ HypnoticGaze (attrs `with` Just enemyId)
    RequestedTokens source _ faces | isSource attrs source -> do
      let
        enemyId = fromMaybe (error "missing enemy id") mEnemyId
        shouldDamageEnemy = any
          ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . tokenFace)
          faces
      when shouldDamageEnemy $ do
        healthDamage' <- field EnemyHealthDamage enemyId
        push $ EnemyDamage enemyId $ nonAttack attrs healthDamage'
      pure e
    _ -> HypnoticGaze . (`with` mEnemyId) <$> runMessage msg attrs
