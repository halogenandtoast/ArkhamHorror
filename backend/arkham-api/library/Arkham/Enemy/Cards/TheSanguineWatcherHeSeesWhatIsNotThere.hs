module Arkham.Enemy.Cards.TheSanguineWatcherHeSeesWhatIsNotThere (theSanguineWatcherHeSeesWhatIsNotThere) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher.Base
import Arkham.Matcher.Investigator
import Arkham.Matcher.Window
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TheSanguineWatcherHeSeesWhatIsNotThere = TheSanguineWatcherHeSeesWhatIsNotThere EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSanguineWatcherHeSeesWhatIsNotThere :: EnemyCard TheSanguineWatcherHeSeesWhatIsNotThere
theSanguineWatcherHeSeesWhatIsNotThere =
  enemy
    TheSanguineWatcherHeSeesWhatIsNotThere
    Cards.theSanguineWatcherHeSeesWhatIsNotThere
    (4, Static 4, 4)
    (2, 2)

instance HasAbilities TheSanguineWatcherHeSeesWhatIsNotThere where
  getAbilities (TheSanguineWatcherHeSeesWhatIsNotThere a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a <> EnemyWithAnyScarletKey)

instance RunMessage TheSanguineWatcherHeSeesWhatIsNotThere where
  runMessage msg e@(TheSanguineWatcherHeSeesWhatIsNotThere attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      chooseOneAtATimeM iid do
        targets skeys \k -> shift k >> shift k
      pure e
    _ -> TheSanguineWatcherHeSeesWhatIsNotThere <$> liftRunMessage msg attrs
