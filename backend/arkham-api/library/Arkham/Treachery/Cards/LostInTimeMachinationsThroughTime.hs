module Arkham.Treachery.Cards.LostInTimeMachinationsThroughTime (
  lostInTimeMachinationsThroughTime,
) where

import Arkham.Asset.Types (Field (..))
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInTimeMachinationsThroughTime = LostInTimeMachinationsThroughTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTimeMachinationsThroughTime :: TreacheryCard LostInTimeMachinationsThroughTime
lostInTimeMachinationsThroughTime =
  treachery LostInTimeMachinationsThroughTime Cards.lostInTimeMachinationsThroughTime

instance RunMessage LostInTimeMachinationsThroughTime where
  runMessage msg t@(LostInTimeMachinationsThroughTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> AssetNonStory <> AssetCanLeavePlayByNormalMeans
      if notNull assets
        then chooseOneM iid do
          targets assets \aid -> do
            dmg <- field AssetDamage aid
            when (dmg > 0) $ moveTokens attrs aid iid #damage dmg
            hrr <- field AssetHorror aid
            when (hrr > 0) $ moveTokens attrs aid iid #horror hrr
            shuffleIntoDeck iid aid
        else repeated 3 $ chooseAndDiscardCard iid attrs
      pure t
    _ -> LostInTimeMachinationsThroughTime <$> liftRunMessage msg attrs
