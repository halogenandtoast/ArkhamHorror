module Arkham.Event.Cards.HypnoticGaze2 (
  hypnoticGaze2,
  HypnoticGaze2 (..),
) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Enemy.Types hiding (EnemyDamage)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {selectedEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype HypnoticGaze2 = HypnoticGaze2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

hypnoticGaze2 :: EventCard HypnoticGaze2
hypnoticGaze2 =
  event (HypnoticGaze2 . (`with` Metadata Nothing)) Cards.hypnoticGaze2

dropUntilAttack :: [Message] -> [Message]
dropUntilAttack = dropWhile (notElem AttackMessage . messageType)

instance RunMessage HypnoticGaze2 where
  runMessage msg e@(HypnoticGaze2 (attrs `With` meta)) = case msg of
    InvestigatorPlayEvent iid eventId _ _ _ | eventId == toId attrs -> do
      enemyId <- fromQueue $ \queue -> case dropUntilAttack queue of
        PerformEnemyAttack details : _ -> attackEnemy details
        _ -> error "unhandled"
      ignoreWindow <-
        checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        [ CancelNext (toSource attrs) AttackMessage
        , RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
        , ignoreWindow
        ]
      pure $ HypnoticGaze2 (attrs `with` Metadata (Just enemyId))
    RequestedChaosTokens source (Just iid) faces | isSource attrs source -> do
      push $ ResetChaosTokens (toSource attrs)
      let
        enemyId = fromMaybe (error "missing enemy id") (selectedEnemy meta)
        shouldDamageEnemy =
          any
            ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . chaosTokenFace)
            faces
      when shouldDamageEnemy $ do
        healthDamage' <- field EnemyHealthDamage enemyId
        sanityDamage' <- field EnemySanityDamage enemyId
        player <- getPlayer iid
        pushWhen (healthDamage' > 0 || sanityDamage' > 0)
          $ If
            (Window.RevealChaosTokenEventEffect (eventOwner attrs) faces (toId attrs))
            [ chooseOrRunOne player
                $ [ Label
                    "Deal health damage"
                    [EnemyDamage enemyId $ nonAttack attrs healthDamage']
                  | healthDamage' > 0
                  ]
                <> [ Label
                    "Deal sanity damage"
                    [EnemyDamage enemyId $ nonAttack attrs sanityDamage']
                   | sanityDamage' > 0
                   ]
            ]
      pure e
    _ -> HypnoticGaze2 . (`with` meta) <$> runMessage msg attrs
