module Arkham.Story.Cards.AnotherWay (AnotherWay (..), anotherWay) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Source
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
    ResolveStory iid _ story' | story' == toId attrs -> do
      alejandro <- selectJust $ enemyIs Enemies.alejandroVela
      chooseOneM iid do
        labeled "I could never turn my back on humanity" do
          exhaustThis alejandro
          disengageFromAll alejandro
          gameModifier attrs iid $ CannotParleyWith $ enemyIs Enemies.alejandroVela
        labeled "I accept" do
          push $ RemoveEnemy alejandro
          push $ AdvanceToAct 1 Acts.timelock A (toSource attrs)
          gameModifiers
            attrs
            iid
            [ CannotParleyWith $ enemyIs Enemies.ichtacaScionOfYig
            , CannotBeAttackedBy $ EnemyWithTrait Trait.Cultist
            , CannotBeEngagedBy $ EnemyWithTrait Trait.Cultist
            ]
      pure s
    _ -> AnotherWay <$> liftRunMessage msg attrs
