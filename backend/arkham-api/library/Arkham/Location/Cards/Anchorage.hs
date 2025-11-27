module Arkham.Location.Cards.Anchorage (anchorage) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.OnThinIce.Helpers

newtype Anchorage = Anchorage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anchorage :: LocationCard Anchorage
anchorage = location Anchorage Cards.anchorage 2 (PerPlayer 1)

instance HasAbilities Anchorage where
  getAbilities (Anchorage a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists HollowedCard) actionAbility
      , scenarioI18n $ withI18nTooltip "anchorage.resign" $ locationResignAction a
      ]

instance RunMessage Anchorage where
  runMessage msg l@(Anchorage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hollows <- select HollowedCard
      let withOwners = hollows & mapMaybe (\c -> (,c) <$> c.owner)
      chooseOneM iid $ for_ withOwners \(owner, c) -> targeting c $ drawCard owner c
      pure l
    _ -> Anchorage <$> liftRunMessage msg attrs
