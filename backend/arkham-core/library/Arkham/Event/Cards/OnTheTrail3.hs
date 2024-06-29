module Arkham.Event.Cards.OnTheTrail3 (onTheTrail3, OnTheTrail3 (..)) where

import Arkham.Discover
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (getCanDiscoverClues)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Id
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Movement
import Arkham.Projection

newtype OnTheTrail3 = OnTheTrail3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheTrail3 :: EventCard OnTheTrail3
onTheTrail3 = event OnTheTrail3 Cards.onTheTrail3

instance RunMessage OnTheTrail3 where
  runMessage msg e@(OnTheTrail3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyAt $ not_ $ locationWithInvestigator iid
      chooseOne
        iid
        [targetLabel enemy [handleTargetChoice iid attrs enemy, DoStep 2 msg] | enemy <- enemies]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      forField EnemyLocation eid \lid -> do
        let doMove = (move attrs iid lid) {moveMeans = Towards}
        push $ Move $ doMove {moveAfter = [Move doMove]}
      pure e
    MoveTo movement | moveSource movement == toSource attrs -> do
      case moveDestination movement of
        ToLocation lid -> pure . OnTheTrail3 $ attrs & setMeta (lid : toResultDefault [] attrs.meta)
        _ -> pure e
    DoStep 2 (PlayThisEvent iid (is attrs -> True)) -> do
      for_ (toResultDefault [] attrs.meta) \lid -> do
        pushWhenM (getCanDiscoverClues NotInvestigate iid lid) $ DiscoverClues iid $ discover lid attrs 1
      pure e
    _ -> OnTheTrail3 <$> lift (runMessage msg attrs)
