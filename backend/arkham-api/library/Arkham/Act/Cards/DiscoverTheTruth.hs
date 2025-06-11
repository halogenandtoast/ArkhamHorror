module Arkham.Act.Cards.DiscoverTheTruth (discoverTheTruth) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype DiscoverTheTruth = DiscoverTheTruth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discoverTheTruth :: ActCard DiscoverTheTruth
discoverTheTruth = act (3, G) DiscoverTheTruth Cards.discoverTheTruth Nothing

instance HasAbilities DiscoverTheTruth where
  getAbilities = actAbilities' G \a ->
    [ restricted
        a
        1
        (exists $ assetIs Assets.clintonFreemanShouldHaveStayedHome <> AssetWithClues (AtLeast $ PerPlayer 1))
        $ Objective
        $ forced AnyWindow
    , restricted a 2 (exists $ locationIs Locations.loadingDocks <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage DiscoverTheTruth where
  runMessage msg a@(DiscoverTheTruth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      selectEach (assetIs Assets.clintonFreemanShouldHaveStayedHome) (removeCluesFrom (attrs.ability 1) n)
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      clintonInPlay <- selectAny $ assetIs Assets.clintonFreemanShouldHaveStayedHome
      veda <- getSetAsideCard Assets.vedaWhitsleySkilledBotanist
      iids <-
        select
          $ NearestToLocation
          $ if clintonInPlay
            then LocationWithAsset $ assetIs Assets.relicOfAgesADeviceOfSomeSort
            else locationIs Locations.loadingDocks
      leadChooseOrRunOneM $ targets iids (`takeControlOfSetAsideAsset` veda)
      deckCount <- getActDecksInPlayCount
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> DiscoverTheTruth <$> liftRunMessage msg attrs
