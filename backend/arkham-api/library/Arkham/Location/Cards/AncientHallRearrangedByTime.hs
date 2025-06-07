module Arkham.Location.Cards.AncientHallRearrangedByTime (ancientHallRearrangedByTime) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token

newtype AncientHallRearrangedByTime = AncientHallRearrangedByTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientHallRearrangedByTime :: LocationCard AncientHallRearrangedByTime
ancientHallRearrangedByTime =
  location AncientHallRearrangedByTime Cards.ancientHallRearrangedByTime 3 (PerPlayer 1)
    & setLabel "ancientHall"
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities AncientHallRearrangedByTime where
  getAbilities (AncientHallRearrangedByTime a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> thisExists a LocationWithAnyDoom) actionAbility
      , groupLimit PerRound
          $ restricted a 2 (Here <> HasSupply Compass <> thisExists a LocationWithAnyDoom) actionAbility
      ]

instance RunMessage AncientHallRearrangedByTime where
  runMessage msg l@(AncientHallRearrangedByTime attrs) = runQueueT $ case msg of
    PlacedLocation _ _ lid | lid == attrs.id -> do
      AncientHallRearrangedByTime <$> liftRunMessage msg (attrs & tokensL %~ addTokens #doom 2)
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      when (attrs.clues > 0) $ flipCluesToDoom attrs 1
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      when (attrs.clues > 0) $ flipCluesToDoom attrs 1
      pure l
    _ -> AncientHallRearrangedByTime <$> liftRunMessage msg attrs
