module Arkham.Location.Cards.TheWhiteShip (theWhiteShip, TheWhiteShip (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype TheWhiteShip = TheWhiteShip LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWhiteShip :: LocationCard TheWhiteShip
theWhiteShip = location TheWhiteShip Cards.theWhiteShip 1 (Static 0)

instance HasModifiersFor TheWhiteShip where
  getModifiersFor (InvestigatorTarget iid) (TheWhiteShip a) = do
    alarmLevel <- getAlarmLevel iid
    pure $ toModifiers a [CannotEnter a.id | alarmLevel >= 5]
  getModifiersFor _ _ = pure []

instance HasAbilities TheWhiteShip where
  getAbilities (TheWhiteShip attrs) =
    extendRevealed attrs [mkAbility attrs 1 $ forced $ RevealLocation #after Anyone $ be attrs]

instance RunMessage TheWhiteShip where
  runMessage msg l@(TheWhiteShip attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      theCaptain <- getSetAsideCard Assets.theCaptain
      assetId <- getRandom
      push $ CreateAssetAt assetId theCaptain (AtLocation attrs.id)
      pure l
    _ -> TheWhiteShip <$> runMessage msg attrs
