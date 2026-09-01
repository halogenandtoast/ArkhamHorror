module Arkham.Location.Cards.TheDrownedCity.TheDrownedQuarter.BlastedRuinsSunkenCircle (blastedRuinsSunkenCircle) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (increaseThisFloodLevel)
import Arkham.ForMovement
import Arkham.Location.CardDefs.TheDrownedCity.TheDrownedQuarter qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BlastedRuinsSunkenCircle = BlastedRuinsSunkenCircle LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedRuinsSunkenCircle :: LocationCard BlastedRuinsSunkenCircle
blastedRuinsSunkenCircle = location BlastedRuinsSunkenCircle Cards.blastedRuinsSunkenCircle 2 (PerPlayer 1)

instance HasAbilities BlastedRuinsSunkenCircle where
  getAbilities (BlastedRuinsSunkenCircle a) =
    if a.revealed
      then
        extendRevealed
          a
          [ restricted a 1 Here $ forced $ TurnEnds #after You
          , groupLimit PerRound
              $ restricted a 2 (exists $ orConnected NotForMovement a <> FloodedLocation)
              $ actionAbilityWithCost (GroupResourceCost (Static 5) (be a))
          ]
      else extendUnrevealed1 a $ mkAbility a 3 $ forced $ Enters #when You (be a)

instance RunMessage BlastedRuinsSunkenCircle where
  runMessage msg l@(BlastedRuinsSunkenCircle attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ IncreaseFloodLevel attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locations <- select $ orConnected NotForMovement attrs <> FloodedLocation
      chooseTargetM iid locations \lid -> push $ DecreaseFloodLevel lid
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      increaseThisFloodLevel attrs
      floodable <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid floodable increaseThisFloodLevel
      pure l
    _ -> BlastedRuinsSunkenCircle <$> liftRunMessage msg attrs
