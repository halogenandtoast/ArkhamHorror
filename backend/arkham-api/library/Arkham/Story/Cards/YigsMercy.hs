module Arkham.Story.Cards.YigsMercy (yigsMercy) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait qualified as Trait

newtype YigsMercy = YigsMercy StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yigsMercy :: StoryCard YigsMercy
yigsMercy = story YigsMercy Cards.yigsMercy

instance RunMessage YigsMercy where
  runMessage msg s@(YigsMercy attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      ichtaca <- selectJust $ enemyIs Enemies.ichtacaScionOfYig
      yigsFury <- getRecordCount YigsFury
      chooseOneM iid do
        if yigsFury >= 16
          then labeled "Ichtaca refuses your plea" nothing
          else do
            labeled "I could never turn my back on humanity" do
              exhaustThis ichtaca
              disengageFromAll ichtaca
              gameModifier attrs iid $ CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig
            labeled "I accept" do
              push $ RemoveEnemy ichtaca
              push $ AdvanceToAct 1 Acts.timelock A (toSource attrs)
              gameModifiers
                attrs
                iid
                [ CannotParleyWith $ enemyIs Enemies.alejandroVela
                , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
                , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
                ]
      pure s
    _ -> YigsMercy <$> liftRunMessage msg attrs
