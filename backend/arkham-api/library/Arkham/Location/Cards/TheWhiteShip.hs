module Arkham.Location.Cards.TheWhiteShip (theWhiteShip, TheWhiteShip (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers

newtype TheWhiteShip = TheWhiteShip LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWhiteShip :: LocationCard TheWhiteShip
theWhiteShip = location TheWhiteShip Cards.theWhiteShip 1 (Static 0)

instance HasModifiersFor TheWhiteShip where
  getModifiersFor (TheWhiteShip a) = modifySelectMaybe a Anyone \iid -> do
    alarmLevel <- lift $ getAlarmLevel iid
    guard $ alarmLevel >= 5
    pure [CannotEnter a.id]

instance HasAbilities TheWhiteShip where
  getAbilities (TheWhiteShip attrs) =
    extendRevealed1 attrs $ mkAbility attrs 1 $ forced $ RevealLocation #after Anyone $ be attrs

instance RunMessage TheWhiteShip where
  runMessage msg l@(TheWhiteShip attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      theCaptain <- getSetAsideCard Assets.theCaptain
      assetId <- getRandom
      push $ CreateAssetAt assetId theCaptain (AtLocation attrs.id)
      pure l
    _ -> TheWhiteShip <$> liftRunMessage msg attrs
