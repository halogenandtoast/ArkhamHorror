{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Act.Cards.MagicAndScience (magicAndScience) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Direction
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Label
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Control.Monad.Extra (findM)
import Data.Function (on)
import Data.Monoid (Last (..))

newtype MagicAndScience = MagicAndScience ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

magicAndScience :: ActCard MagicAndScience
magicAndScience =
  act (2, A) MagicAndScience Cards.magicAndScience
    $ Just
    $ GroupClueCost (PerPlayer 2) (location_ "Chamber of Time")

instance HasAbilities MagicAndScience where
  getAbilities (MagicAndScience a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

data LocationCandidate = LocationCandidate
  { id :: LocationId
  , doom :: Int
  , card :: Card
  }

instance RunMessage MagicAndScience where
  runMessage msg a@(MagicAndScience attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore iid (attrs.ability 1) $ mapOneOf CardWithPrintedLocationSymbol locationSymbols
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      chamberOfTime <- selectJust $ location_ "Chamber of Time"
      relicOfAges <-
        maybe
          (Left <$> fetchCard (SetAsideCardMatch $ cardIs Assets.relicOfAgesADeviceOfSomeSort))
          (pure . Right)
          =<< selectOne (assetIs Assets.relicOfAgesADeviceOfSomeSort)
      investigators <- select $ investigatorAt chamberOfTime
      entryway <- selectJust $ location_ "Entryway"
      chooseOrRunOneM lead do
        targets investigators \iid ->
          either (takeControlOfSetAsideAsset iid) (takeControlOfAsset iid) relicOfAges
      push $ SetConnections chamberOfTime []
      setLocationLabel chamberOfTime "pos1"
      push $ SetConnections entryway []
      n <- selectCount Anywhere
      setLocationLabel entryway ("pos" <> tshow n)
      otherLocations <- select $ not_ $ LocationMatchAny [be chamberOfTime, be entryway]
      for_ otherLocations (push . (`SetConnections` []))
      candidates <- for otherLocations $ \location -> do
        LocationCandidate location <$> field LocationDoom location <*> field LocationCard location
      let candidateGroups = groupBy ((==) `on` (.doom)) $ sortBy (compare `on` (.doom)) candidates
      for_ candidateGroups \case
        [] -> error "Unhandled"
        [c] -> handleTarget lead attrs c.id
        cs -> focusCards (map (.card) cs) do
          chooseOneAtATimeM lead do
            for_ cs \c -> targeting c.card $ handleTarget lead attrs c.id

      push $ NextAdvanceActStep (toId attrs) 1
      advanceActDeck attrs
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (LocationTarget lid) -> do
      -- find first unfilled position
      x <- selectCount Anywhere
      let toPosition = ("pos" <>) . tshow @Int
      let positions = map (toFst toPosition) [2 .. (x - 1)]
      mpositition <- findM (selectNone . LocationWithLabel . Label . fst) positions
      case mpositition of
        Nothing -> error "invalid logic"
        Just (pos, n) -> do
          let left = toPosition (n - 1)
          leftLocation <- selectJust $ LocationWithLabel $ Label left
          setLocationLabel lid pos
          push $ PlacedLocationDirection lid RightOf leftLocation
          pure a
    NextAdvanceActStep actId 1 | toId attrs == actId -> do
      -- find last filled position
      n <- selectCount Anywhere
      let toPosition = ("pos" <>) . tshow @Int
      let positions = map toPosition [2 .. (n - 1)]
      mposition <- getLast . foldMap Last <$> traverse (selectOne . LocationWithLabel . Label) positions
      case mposition of
        Nothing -> error "invalid logic"
        Just pos -> do
          entryway <- selectJust $ location_ "Entryway"
          push $ PlacedLocationDirection entryway RightOf pos
          pure a
    _ -> MagicAndScience <$> liftRunMessage msg attrs
