module Arkham.Enemy.Cards.BurrowingHybrid (burrowingHybrid) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Direction
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WrittenInRock.Helpers

newtype BurrowingHybrid = BurrowingHybrid EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burrowingHybrid :: EnemyCard BurrowingHybrid
burrowingHybrid = enemy BurrowingHybrid Cards.burrowingHybrid (3, Static 3, 3) (1, 1)

instance HasModifiersFor BurrowingHybrid where
  getModifiersFor (BurrowingHybrid a) = do
    mineCart <- selectJust (assetIs Assets.mineCartReliableButBroken)
    meta <- field AssetMeta mineCart
    let dir = toResultDefault East meta
    loc <- selectJust (locationWithAsset mineCart)
    pos <- fieldJust LocationPosition loc
    madjacent <- selectOne (LocationInPosition (updatePosition pos dir))
    for_ madjacent \adjacent -> do
      rails <- getRails adjacent
      when (oppositeDirection dir `elem` rails) do
        whenMatch adjacent (oneOf [LocationCanBeSlid, LocationCanBeSwapped]) do
          modifySelf a [ScenarioModifier "active"]

instance HasAbilities BurrowingHybrid where
  getAbilities (BurrowingHybrid a) =
    extend1 a
      $ restricted a 1 (thisExists a (EnemyWithModifier (ScenarioModifier "active")))
      $ forced
      $ EnemyEntersPlay #after (be a)

instance RunMessage BurrowingHybrid where
  runMessage msg e@(BurrowingHybrid attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      mineCart <- selectJust (assetIs Assets.mineCartReliableButBroken)
      meta <- field AssetMeta mineCart
      let dir = toResultDefault East meta
      loc <- selectJust (locationWithAsset mineCart)
      pos <- (`updatePosition` dir) <$> fieldJust LocationPosition loc
      madjacent <- selectOne (LocationInPosition pos)
      for_ madjacent \lid -> do
        slideLocations <-
          matches lid LocationCanBeSlid >>= \case
            False -> pure []
            True -> getEmptyPositions lid
        case slideLocations of
          [] -> do
            swappable <- select $ mapOneOf LocationInPosition (adjacentPositions pos) <> LocationCanBeSwapped
            leadChooseOneM do
              targets swappable (swapLocations lid)
          xs -> leadChooseOneM do
            for_ xs \newPos ->
              gridLabeled (gridLabel newPos) $ push $ PlaceGrid (GridLocation newPos lid)
      pure e
    _ -> BurrowingHybrid <$> liftRunMessage msg attrs
