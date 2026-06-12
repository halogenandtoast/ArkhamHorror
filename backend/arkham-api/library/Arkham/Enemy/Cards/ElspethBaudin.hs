module Arkham.Enemy.Cards.ElspethBaudin (elspethBaudin) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.BadBlood.Helpers
import Arkham.Scenarios.BadBlood.Meta

newtype ElspethBaudin = ElspethBaudin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elspethBaudin :: EnemyCard ElspethBaudin
elspethBaudin = enemy ElspethBaudin Cards.elspethBaudin (8, PerPlayer 4, 8) (2, 2)

instance HasModifiersFor ElspethBaudin where
  getModifiersFor (ElspethBaudin a) = do
    n <- (.elspethMemories) <$> getBadBloodMeta
    when (n > 0) $ modifySelf a [EnemyFight (-n), EnemyEvade (-n)]

instance HasAbilities ElspethBaudin where
  getAbilities (ElspethBaudin a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)

instance RunMessage ElspethBaudin where
  runMessage msg e@(ElspethBaudin attrs) = runQueueT $ case msg of
    Msg.EnemyEvaded _ eid | eid == attrs.id -> do
      viaEvasion <-
        andM
          [ (== Just #evade) <$> getSkillTestAction
          , (== Just attrs.id) <$> getSkillTestTargetedEnemy
          ]
      if viaEvasion
        then ElspethBaudin <$> liftRunMessage msg attrs
        else pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs.id
      healAllDamage (attrs.ability 1) attrs
      scenarioI18n $ scope "elspethBaudin" $ flavor $ h "title" >> p "body"
      whenJustM (selectOne agnesBaker) \agnes -> do
        n <- (.elspethMemories) <$> getBadBloodMeta
        clues <- perPlayer 2
        scenarioI18n $ chooseOrRunOneM agnes do
          when (n > 0) $ labeled' "collectMemoryFromElspeth" agnesStealsMemory
          countVar clues $ labeled' "gainCluesFromTokenBank" $ gainClues agnes (attrs.ability 1) clues
      exhaustThis attrs
      disengageFromAll attrs
      pure e
    _ -> ElspethBaudin <$> liftRunMessage msg attrs
