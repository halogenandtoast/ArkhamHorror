module Arkham.Enemy.Cards.AbarranArrigorriagakoaAbarranUnleashed (abarranArrigorriagakoaAbarranUnleashed) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype AbarranArrigorriagakoaAbarranUnleashed = AbarranArrigorriagakoaAbarranUnleashed EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abarranArrigorriagakoaAbarranUnleashed :: EnemyCard AbarranArrigorriagakoaAbarranUnleashed
abarranArrigorriagakoaAbarranUnleashed =
  enemy
    AbarranArrigorriagakoaAbarranUnleashed
    Cards.abarranArrigorriagakoaAbarranUnleashed
    (4, PerPlayer 4, 4)
    (2, 2)

instance HasModifiersFor AbarranArrigorriagakoaAbarranUnleashed where
  getModifiersFor (AbarranArrigorriagakoaAbarranUnleashed a) = do
    alarm <- getMaxAlarmLevel
    modifySelfWhen a (alarm >= 5) [AddKeyword Keyword.Hunter]

instance HasAbilities AbarranArrigorriagakoaAbarranUnleashed where
  getAbilities (AbarranArrigorriagakoaAbarranUnleashed a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) (SourceOwnedBy You)

instance RunMessage AbarranArrigorriagakoaAbarranUnleashed where
  runMessage msg e@(AbarranArrigorriagakoaAbarranUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid NoMulligan 1
      pure e
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      suits <- cards & mapMaybeM toPlayingCard <&> map (.suit)
      when (Diamonds `elem` suits) do
        initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> AbarranArrigorriagakoaAbarranUnleashed <$> liftRunMessage msg attrs
