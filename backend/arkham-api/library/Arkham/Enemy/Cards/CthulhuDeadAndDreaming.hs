module Arkham.Enemy.Cards.CthulhuDeadAndDreaming (cthulhuDeadAndDreaming) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.SepulchreOfTheSleeper.Helpers

newtype CthulhuDeadAndDreaming = CthulhuDeadAndDreaming EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuDeadAndDreaming :: EnemyCard CthulhuDeadAndDreaming
cthulhuDeadAndDreaming =
  enemyWith CthulhuDeadAndDreaming Cards.cthulhuDeadAndDreaming $ spawnAtL ?~ "Dreamer's Rest"

instance HasAbilities CthulhuDeadAndDreaming where
  getAbilities (CthulhuDeadAndDreaming a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEvaded #after You (be a)

instance RunMessage CthulhuDeadAndDreaming where
  runMessage msg e@(CthulhuDeadAndDreaming attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "Either ready him, or each investigator at Cthulhu's location takes 1
      -- direct damage." The investigator who evaded him makes the call.
      chooseOneM iid do
        labeled' "readyCthulhu" $ ready attrs
        labeled' "eachInvestigatorHereTakesDirectDamage" do
          selectEach (investigatorAt $ locationWithEnemy attrs) \iid' ->
            directDamage iid' (attrs.ability 1) 1
      pure e
    _ -> CthulhuDeadAndDreaming <$> liftRunMessage msg attrs
