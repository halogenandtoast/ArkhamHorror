module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Locations.GrandHallRuined (grandHallRuined) where

import Arkham.Ability
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GrandHallRuined = GrandHallRuined LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandHallRuined :: LocationCard GrandHallRuined
grandHallRuined = location GrandHallRuined Cards.grandHallRuined 2 (Static 0)

instance HasAbilities GrandHallRuined where
  getAbilities (GrandHallRuined a) =
    extendRevealed
      a
      [ locationResignAction a
      , scenarioI18n
          $ withI18nTooltip "grandHallRuined.placeClue"
          $ mkAbility a 1
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage GrandHallRuined where
  runMessage msg l@(GrandHallRuined attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs 1
      pure l
    _ -> GrandHallRuined <$> liftRunMessage msg attrs
