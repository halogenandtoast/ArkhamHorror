module Arkham.Event.Cards.HypnoticGaze
  ( hypnoticGaze
  , HypnoticGaze(..)
  ) where

import Arkham.Prelude

import Arkham.Attack
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
import Arkham.RequestedChaosTokenStrategy
import Arkham.Timing qualified as Timing
import Arkham.ChaosToken
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Metadata = Metadata { selectedEnemy :: Maybe EnemyId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HypnoticGaze = HypnoticGaze (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze :: EventCard HypnoticGaze
hypnoticGaze =
  event (HypnoticGaze . (`with` Metadata Nothing)) Cards.hypnoticGaze

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance RunMessage HypnoticGaze where
  runMessage msg e@(HypnoticGaze (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eventId _ _ _ | eventId == toId attrs -> do
      enemyId <- fromQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack details : _ -> attackEnemy details
        _ -> error "unhandled"
      ignoreWindow <- checkWindows
        [ Window
            Timing.After
            (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)
        ]
      pushAll
        [ CancelNext (toSource attrs) AttackMessage
        , RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
        , ignoreWindow
        ]
      pure $ HypnoticGaze (attrs `with` Metadata (Just enemyId))
    RequestedChaosTokens source _ faces | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      let
        enemyId = fromMaybe (error "missing enemy id") (selectedEnemy meta)
        shouldDamageEnemy = any
          ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace)
          faces
      when shouldDamageEnemy $ do
        healthDamage' <- field EnemyHealthDamage enemyId
        when (healthDamage' > 0) $ push $ If
          (Window.RevealChaosTokenEventEffect (eventOwner attrs) faces (toId attrs))
          [EnemyDamage enemyId $ nonAttack attrs healthDamage']
      pure e
    _ -> HypnoticGaze . (`with` meta) <$> runMessage msg attrs
