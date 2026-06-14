module Arkham.Location.Cards.BlastedRuinsCrumblingEdifices (blastedRuinsCrumblingEdifices) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BlastedRuinsCrumblingEdifices = BlastedRuinsCrumblingEdifices LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedRuinsCrumblingEdifices :: LocationCard BlastedRuinsCrumblingEdifices
blastedRuinsCrumblingEdifices = location BlastedRuinsCrumblingEdifices Cards.blastedRuinsCrumblingEdifices 2 (Static 1)

instance HasAbilities BlastedRuinsCrumblingEdifices where
  getAbilities (BlastedRuinsCrumblingEdifices a) =
    extendUnrevealed1 a (mkAbility a 3 $ forced $ Enters #when You (be a))
      <> extendRevealed
        a
        [ restricted a 1 Here $ forced $ TurnEnds #after You
        , groupLimit PerRound
            $ restricted a 2 (exists $ orConnected NotForMovement a <> FloodedLocation)
            $ actionAbilityWithCost (GroupDiscardCost (PerPlayer 1) #any (be a))
        ]

instance RunMessage BlastedRuinsCrumblingEdifices where
  runMessage msg l@(BlastedRuinsCrumblingEdifices attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ IncreaseFloodLevel attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select $ orConnected NotForMovement attrs <> FloodedLocation
      chooseTargetM iid locations \lid -> push $ DecreaseFloodLevel lid
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      push $ IncreaseFloodLevel attrs.id
      adjacent <- select $ connectedFrom (be attrs) <> not_ FullyFloodedLocation
      chooseTargetM iid adjacent $ push . IncreaseFloodLevel . asId
      pure l
    _ -> BlastedRuinsCrumblingEdifices <$> liftRunMessage msg attrs
