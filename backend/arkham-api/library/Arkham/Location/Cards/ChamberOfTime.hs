module Arkham.Location.Cards.ChamberOfTime (chamberOfTime, ChamberOfTime (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype ChamberOfTime = ChamberOfTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chamberOfTime :: LocationCard ChamberOfTime
chamberOfTime =
  locationWith ChamberOfTime Cards.chamberOfTime 4 (PerPlayer 2) (connectsToL .~ singleton RightOf)

instance HasAbilities ChamberOfTime where
  getAbilities (ChamberOfTime attrs) =
    extendRevealed1 attrs
      $ mkAbility attrs 1
      $ forced
      $ PutLocationIntoPlay #after Anyone (be attrs)

instance RunMessage ChamberOfTime where
  runMessage msg l@(ChamberOfTime attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mRelicOfAges <-
        getSetAsideCardsMatching
          (mapOneOf cardIs [Assets.relicOfAgesRepossessThePast, Assets.relicOfAgesADeviceOfSomeSort])
      case listToMaybe mRelicOfAges of
        Nothing -> error "Missing relic of ages"
        Just relicOfAges -> do
          assetId <- getRandom
          pushAll
            [ CreateAssetAt assetId relicOfAges (AttachedToLocation $ toId attrs)
            , PlaceDoom (attrs.ability 1) (toTarget attrs) 1
            ]
      pure l
    _ -> ChamberOfTime <$> runMessage msg attrs
