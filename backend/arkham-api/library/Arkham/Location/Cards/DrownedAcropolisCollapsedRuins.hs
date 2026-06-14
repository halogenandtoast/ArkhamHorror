module Arkham.Location.Cards.DrownedAcropolisCollapsedRuins (drownedAcropolisCollapsedRuins) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (decreaseThisFloodLevel, increaseThisFloodLevel)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DrownedAcropolisCollapsedRuins = DrownedAcropolisCollapsedRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedAcropolisCollapsedRuins :: LocationCard DrownedAcropolisCollapsedRuins
drownedAcropolisCollapsedRuins = location DrownedAcropolisCollapsedRuins Cards.drownedAcropolisCollapsedRuins 3 (Static 2)

instance HasAbilities DrownedAcropolisCollapsedRuins where
  getAbilities (DrownedAcropolisCollapsedRuins a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #after You
      , limited (GroupLimit PerGame 2) $ restricted a 2 Here actionAbility
      ]

instance RunMessage DrownedAcropolisCollapsedRuins where
  runMessage msg l@(DrownedAcropolisCollapsedRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      adjacent <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseOrRunOneM iid $ withI18n do
        targets adjacent increaseThisFloodLevel
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      increaseThisFloodLevel attrs
      floodedLocations <- select FloodedLocation
      chooseOneM iid $ targets floodedLocations decreaseThisFloodLevel
      pure l
    _ -> DrownedAcropolisCollapsedRuins <$> liftRunMessage msg attrs
