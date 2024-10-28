module Arkham.Location.Cards.SouthChurch_299 (southChurch_299, SouthChurch_299 (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype SouthChurch_299 = SouthChurch_299 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_299 :: LocationCard SouthChurch_299
southChurch_299 = location SouthChurch_299 Cards.southChurch_299 2 (Static 0)

instance HasAbilities SouthChurch_299 where
  getAbilities (SouthChurch_299 attrs) =
    extendRevealed
      attrs
      [ restricted attrs 1 (withBreaches attrs Here)
          $ actionAbilityWithCost (DiscardAssetCost AnyAsset)
      , withTooltip "You hide through the night." $ locationResignAction attrs
      ]

instance RunMessage SouthChurch_299 where
  runMessage msg l@(SouthChurch_299 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      let breachCount = countLocationBreaches attrs
      act <- selectJust AnyAct
      pushAll
        [ RemoveBreaches (toTarget attrs) breachCount
        , PlaceBreaches (toTarget act) breachCount
        ]
      pure l
    _ -> SouthChurch_299 <$> liftRunMessage msg attrs
