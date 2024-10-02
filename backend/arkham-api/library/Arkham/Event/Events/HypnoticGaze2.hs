module Arkham.Event.Events.HypnoticGaze2 (hypnoticGaze2, HypnoticGaze2 (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Enemy.Types hiding (EnemyDamage)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (getPlayer)
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype Metadata = Metadata {selectedEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HypnoticGaze2 = HypnoticGaze2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze2 :: EventCard HypnoticGaze2
hypnoticGaze2 = event (HypnoticGaze2 . (`with` Metadata Nothing)) Cards.hypnoticGaze2

instance RunMessage HypnoticGaze2 where
  runMessage msg e@(HypnoticGaze2 (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let currentAttack = getAttackDetails attrs.windows
      cancelAttack attrs currentAttack
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      cancelledOrIgnoredCardOrGameEffect attrs
      pure $ HypnoticGaze2 (attrs `with` Metadata (Just currentAttack.enemy))
    RequestedChaosTokens (isSource attrs -> True) (Just iid) faces -> do
      let enemyId = fromMaybe (error "missing enemy id") (selectedEnemy meta)
      let valid =
            if tabooed TabooList22 attrs
              then isSymbolChaosToken
              else (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
      when (any (valid . chaosTokenFace) faces) do
        healthDamage' <- field EnemyHealthDamage enemyId
        sanityDamage' <- field EnemySanityDamage enemyId
        player <- getPlayer iid
        pushWhen (healthDamage' > 0 || sanityDamage' > 0)
          $ If
            (Window.RevealChaosTokenEventEffect attrs.owner faces attrs.id)
            [ Msg.chooseOrRunOne player
                $ [ Label "Deal health damage" [EnemyDamage enemyId $ nonAttack attrs healthDamage']
                  | healthDamage' > 0
                  ]
                <> [ Label "Deal sanity damage" [EnemyDamage enemyId $ nonAttack attrs sanityDamage']
                   | sanityDamage' > 0
                   ]
            ]
      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> HypnoticGaze2 . (`with` meta) <$> liftRunMessage msg attrs
