module Arkham.Location.Cards.OuterFieldsBloodiedPaths (outerFieldsBloodiedPaths) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype OuterFieldsBloodiedPaths = OuterFieldsBloodiedPaths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outerFieldsBloodiedPaths :: LocationCard OuterFieldsBloodiedPaths
outerFieldsBloodiedPaths =
  symbolLabel
    $ locationWith
      OuterFieldsBloodiedPaths
      Cards.outerFieldsBloodiedPaths
      2
      (PerPlayer 3)
      connectsToAdjacent

instance HasAbilities OuterFieldsBloodiedPaths where
  getAbilities (OuterFieldsBloodiedPaths a) =
    extendRevealed
      a
      [ groupLimit PerGame $ restricted a 1 (Here <> exists (enemyAt a)) actionAbility
      , groupLimit PerGame
          $ restricted a 2 (exists $ assetIs Assets.theCaptives <> AssetWithDamage)
          $ freeReaction
          $ DiscoveringLastClue #after You (be a)
      ]

instance RunMessage OuterFieldsBloodiedPaths where
  runMessage msg l@(OuterFieldsBloodiedPaths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAt attrs
      chooseTargetM iid enemies \enemy -> do
        connectingLocations <- select $ connectedFrom (be attrs) <> LocationCanBeEnteredBy enemy
        chooseTargetM iid connectingLocations $ enemyMoveTo (attrs.ability 1) enemy
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      selectForMaybeM (assetIs Assets.theCaptives) \captives ->
        healDamage captives (attrs.ability 2) 2
      pure l
    _ -> OuterFieldsBloodiedPaths <$> liftRunMessage msg attrs
