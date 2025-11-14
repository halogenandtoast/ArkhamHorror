module Arkham.Location.Cards.StaffAccessHallway (staffAccessHallway) where

import Arkham.Ability
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StaffAccessHallway = StaffAccessHallway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

staffAccessHallway :: LocationCard StaffAccessHallway
staffAccessHallway =
  symbolLabel
    $ locationWith
      StaffAccessHallway
      Cards.staffAccessHallway
      2
      (Static 0)
      (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 4) YourLocation)

instance HasAbilities StaffAccessHallway where
  getAbilities (StaffAccessHallway a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage StaffAccessHallway where
  runMessage msg l@(StaffAccessHallway attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      createEnemyAtLocationMatching_ Enemies.abarranArrigorriagakoaAbarranUnleashed "Owner's Office"
      shuffleSetAsideIntoEncounterDeck Set.FortunesChosen
      pure l
    _ -> StaffAccessHallway <$> liftRunMessage msg attrs
