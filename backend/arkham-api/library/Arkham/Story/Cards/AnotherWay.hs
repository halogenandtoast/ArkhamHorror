module Arkham.Story.Cards.AnotherWay (anotherWay) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.ShatteredAeons.Helpers
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Trait qualified as Trait

newtype AnotherWay = AnotherWay StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anotherWay :: StoryCard AnotherWay
anotherWay = story AnotherWay Cards.anotherWay

instance RunMessage AnotherWay where
  runMessage msg s@(AnotherWay attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      alejandro <- selectJust $ enemyIs Enemies.alejandroVela
      chooseOneM iid $ scenarioI18n do
        questionLabeledCard attrs
        labeled' "anotherWay.reject" do
          exhaustThis alejandro
          disengageFromAll alejandro
          gameModifier attrs iid $ CannotParleyWith $ enemyIs Enemies.alejandroVela
        labeled' "anotherWay.accept" do
          removeEnemy alejandro
          advanceToAct' attrs 1 Acts.timelock A
          gameModifiers
            attrs
            iid
            [ CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig
            , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
            , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
            ]
      pure s
    _ -> AnotherWay <$> liftRunMessage msg attrs
