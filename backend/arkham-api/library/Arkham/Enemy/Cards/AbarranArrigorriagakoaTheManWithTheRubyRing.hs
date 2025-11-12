module Arkham.Enemy.Cards.AbarranArrigorriagakoaTheManWithTheRubyRing (abarranArrigorriagakoaTheManWithTheRubyRing) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype AbarranArrigorriagakoaTheManWithTheRubyRing = AbarranArrigorriagakoaTheManWithTheRubyRing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abarranArrigorriagakoaTheManWithTheRubyRing :: EnemyCard AbarranArrigorriagakoaTheManWithTheRubyRing
abarranArrigorriagakoaTheManWithTheRubyRing =
  enemyWith
    AbarranArrigorriagakoaTheManWithTheRubyRing
    Cards.abarranArrigorriagakoaTheManWithTheRubyRing
    (0, Static 1, 0)
    (0, 0)
    (\a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing})

instance HasAbilities AbarranArrigorriagakoaTheManWithTheRubyRing where
  getAbilities (AbarranArrigorriagakoaTheManWithTheRubyRing a) = extend a
    [ mkAbility a 1 $ forced $ EnemyEngaged #when You (be a)
    , skillTestAbility $ restricted a 2 OnSameLocation actionAbility
    ]

instance RunMessage AbarranArrigorriagakoaTheManWithTheRubyRing where
  runMessage msg e@(AbarranArrigorriagakoaTheManWithTheRubyRing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push AdvanceCurrentAgenda
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) attrs #agility (Fixed 3)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      raiseAlarmLevel (attrs.ability 2) [iid]
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      checkGameIcons attrs iid NoMulligan 1
      pure e
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      cards' <- cards & mapMaybeM toPlayingCard
      focusCards cards $ continue_ iid
      case cards' of
        (pc : _) -> when (pc.suit `elem` [Hearts, Diamonds]) $ remember StoleAbarransKeys
        _ -> pure ()
      pure e
    _ -> AbarranArrigorriagakoaTheManWithTheRubyRing <$> liftRunMessage msg attrs
