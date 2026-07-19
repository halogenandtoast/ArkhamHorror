module Arkham.Act.Cards.WalkingThroughTime (walkingThroughTime) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Helpers (advancedWithOther)
import Arkham.Act.Import.Lifted hiding (advancedWithOther)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Trait (Trait (Future, Past, Present, Scientist))
import Arkham.Window qualified as Window

newtype WalkingThroughTime = WalkingThroughTime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walkingThroughTime :: ActCard WalkingThroughTime
walkingThroughTime = act (1, A) WalkingThroughTime Cards.walkingThroughTime Nothing

instance HasAbilities WalkingThroughTime where
  getAbilities (WalkingThroughTime a) =
    [ mkAbility a 1
        $ freeReaction
        $ oneOf
          [ Moves
              #after
              You
              AnySource
              (LocationWithAsset $ AssetWithTrait Scientist <> AssetWithTrait time)
              (LocationWithTrait time)
          | time <- [Past, Present, Future]
          ]
    , onlyOnce $ restricted a 2 objectiveMet $ Objective $ forced AnyWindow
    ]
   where
    nonTindalos = not_ (locationIs Locations.tindalos)
    atNonTindalos def = exists (assetIs def <> AssetAt nonTindalos)
    objectiveMet =
      atNonTindalos Assets.thomasCorriganPast
        <> atNonTindalos Assets.maryZielinskiPast
        <> atNonTindalos Assets.thomasCorriganPresent
        <> atNonTindalos Assets.maryZielinskiPresent
        <> atNonTindalos Assets.thomasCorriganFuture
        <> atNonTindalos Assets.maryZielinskiFuture
        <> notExists (StoryMatchAll [])

getMove :: [Window.Window] -> Maybe (LocationId, LocationId)
getMove [] = Nothing
getMove ((Window.windowType -> Window.Moves _ _ (Just from) destination _) : _) = Just (from, destination)
getMove (_ : xs) = getMove xs

instance RunMessage WalkingThroughTime where
  runMessage msg a@(WalkingThroughTime attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getMove -> Just (from, destination)) _ -> do
      scientists <- select $ AssetWithTrait Scientist <> AssetAt (LocationWithId from)
      movable <- filterM (`assetCanEnter` destination) scientists
      when (notNull movable) do
        chooseSomeM iid "Done moving scientists" do
          targets movable \scientist -> place scientist (AtLocation destination)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> WalkingThroughTime <$> liftRunMessage msg attrs
