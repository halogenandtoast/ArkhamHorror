module Arkham.Enemy.Cards.NadiaNimr (nadiaNimr) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers (campaignI18n)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype NadiaNimr = NadiaNimr EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nadiaNimr :: EnemyCard NadiaNimr
nadiaNimr =
  enemyWith NadiaNimr Cards.nadiaNimr
    $ spawnAtL
    ?~ SpawnAt "Temple Courtyard"

instance HasAbilities NadiaNimr where
  getAbilities (NadiaNimr a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage NadiaNimr where
  runMessage msg e@(NadiaNimr attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      campaignI18n $ chooseAmount' iid "nadiaNimr.horrorToTake" "$horror" 0 3 attrs
      pure e
    ResolveAmounts iid (getChoiceAmount "$horror" -> n) (isTarget attrs -> True) -> do
      sid <- getRandom
      when (n > 0) $ assignHorror iid (attrs.ability 1) n
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed $ max 0 (7 - 2 * n))
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      addToVictory iid attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> NadiaNimr <$> liftRunMessage msg attrs
