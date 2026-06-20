module Arkham.Enemy.Cards.JeanDevereuxPossessed (jeanDevereuxPossessed) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (disengageEnemyFromAll)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Scenarios.LaidToRest.Helpers

newtype JeanDevereuxPossessed = JeanDevereuxPossessed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeanDevereuxPossessed :: EnemyCard JeanDevereuxPossessed
jeanDevereuxPossessed =
  enemy JeanDevereuxPossessed Cards.jeanDevereuxPossessed
    & setOnlyPrey
      (oneOf [investigatorIs Investigators.jimCulver, investigatorIs Investigators.jimCulverParallel])

instance HasAbilities JeanDevereuxPossessed where
  getAbilities (JeanDevereuxPossessed a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "jeanDevereuxPossessed.parley"
          $ skillTestAbility
          $ restricted a 1 OnSameLocation parleyAction_
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage JeanDevereuxPossessed where
  runMessage msg e@(JeanDevereuxPossessed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower
        $ SumCalculation
          [Fixed 3, VictoryDisplayCountCalculation (basic $ CardWithTitle "Unfinished Business")]
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      healAllDamage (attrs.ability 1) attrs
      disengageEnemyFromAll attrs
      lead <- getLead
      flipOverBy lead (attrs.ability 1) attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId (toSource attrs) #other
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      seekingClosure <- genCard Cards.jeanDevereuxSeekingClosure
      push $ ReplaceEnemy attrs.id seekingClosure Swap
      pure e
    _ -> JeanDevereuxPossessed <$> liftRunMessage msg attrs
