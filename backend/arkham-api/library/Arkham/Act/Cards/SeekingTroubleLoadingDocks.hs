module Arkham.Act.Cards.SeekingTroubleLoadingDocks (seekingTroubleLoadingDocks) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype SeekingTroubleLoadingDocks = SeekingTroubleLoadingDocks ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingTroubleLoadingDocks :: ActCard SeekingTroubleLoadingDocks
seekingTroubleLoadingDocks = act (2, G) SeekingTroubleLoadingDocks Cards.seekingTroubleLoadingDocks Nothing

instance HasAbilities SeekingTroubleLoadingDocks where
  getAbilities = actAbilities1' G \a ->
    restricted
      a
      1
      (exists $ assetIs Assets.merleGarvinUnhelpfulGuide <> AssetWithClues (AtLeast $ PerPlayer 1))
      $ Objective
      $ forced AnyWindow

instance RunMessage SeekingTroubleLoadingDocks where
  runMessage msg a@(SeekingTroubleLoadingDocks attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      selectEach (assetIs Assets.merleGarvinUnhelpfulGuide) (removeCluesFrom (attrs.ability 1) n)
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      loadingDocks <- placeLocationCard Locations.loadingDocks
      placeTokens attrs loadingDocks #resource 4
      advanceToAct attrs Cards.discoverTheTruth G
      pure a
    _ -> SeekingTroubleLoadingDocks <$> liftRunMessage msg attrs
