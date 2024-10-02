module Arkham.Event.Events.HypnoticGaze (hypnoticGaze, HypnoticGaze (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Enemy.Types hiding (EnemyDamage)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.Taboo
import Arkham.Window qualified as Window

newtype Metadata = Metadata {selectedEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HypnoticGaze = HypnoticGaze (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticGaze :: EventCard HypnoticGaze
hypnoticGaze = event (HypnoticGaze . (`with` Metadata Nothing)) Cards.hypnoticGaze

instance RunMessage HypnoticGaze where
  runMessage msg e@(HypnoticGaze (attrs `With` meta)) = runQueueT case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let currentAttack = getAttackDetails attrs.windows
      cancelAttack attrs currentAttack
      push $ RequestChaosTokens (toSource attrs) (Just iid) (Reveal 1) SetAside
      cancelledOrIgnoredCardOrGameEffect attrs
      pure $ HypnoticGaze (attrs `with` Metadata (Just currentAttack.enemy))
    RequestedChaosTokens (isSource attrs -> True) (Just iid) faces -> do
      continue iid []
      let enemyId = fromMaybe (error "missing enemy id") (selectedEnemy meta)
      let valid =
            if tabooed TabooList21 attrs
              then isSymbolChaosToken
              else (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail])
      let shouldDamageEnemy = any (valid . chaosTokenFace) faces
      healthDamage' <- field EnemyHealthDamage enemyId
      pushWhen (shouldDamageEnemy && healthDamage' > 0) do
        If
          (Window.RevealChaosTokenEventEffect attrs.owner faces attrs.id)
          [EnemyDamage enemyId $ nonAttack attrs healthDamage']
      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> HypnoticGaze . (`with` meta) <$> liftRunMessage msg attrs
