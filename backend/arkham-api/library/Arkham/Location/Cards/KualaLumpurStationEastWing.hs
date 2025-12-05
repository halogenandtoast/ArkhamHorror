module Arkham.Location.Cards.KualaLumpurStationEastWing (kualaLumpurStationEastWing) where

import Arkham.Ability
import Arkham.Card
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.ScenarioLogKey

newtype KualaLumpurStationEastWing = KualaLumpurStationEastWing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kualaLumpurStationEastWing :: LocationCard KualaLumpurStationEastWing
kualaLumpurStationEastWing =
  symbolLabel $ location KualaLumpurStationEastWing Cards.kualaLumpurStationEastWing 3 (PerPlayer 1)

instance HasAbilities KualaLumpurStationEastWing where
  getAbilities (KualaLumpurStationEastWing a) =
    extendRevealed
      a
      [ playerLimit PerTurn
          $ restricted a 1 (Here <> DuringTurn You <> CanMoveTo (ConnectedFrom ForMovement (be a)))
          $ FastAbility (ResourceCost 2)
      , groupLimit PerGame
          $ restricted a 2 Here
          $ actionAbilityWithCost (DiscardAssetCost $ AssetControlledBy You)
      ]

instance RunMessage KualaLumpurStationEastWing where
  runMessage msg l@(KualaLumpurStationEastWing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ ConnectedTo ForMovement (be attrs)
      chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ (discardPayments -> ps) -> do
      for_ (nonEmpty ps) \((_, card) :| _) -> do
        gainResources iid (attrs.ability 2) $ printedCardCost card `div` 2
      remember TradedForAKitten
      pure l
    _ -> KualaLumpurStationEastWing <$> liftRunMessage msg attrs
