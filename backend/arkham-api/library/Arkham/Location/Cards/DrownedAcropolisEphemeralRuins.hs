module Arkham.Location.Cards.DrownedAcropolisEphemeralRuins (drownedAcropolisEphemeralRuins) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (decreaseThisFloodLevel, increaseThisFloodLevel)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DrownedAcropolisEphemeralRuins = DrownedAcropolisEphemeralRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drownedAcropolisEphemeralRuins :: LocationCard DrownedAcropolisEphemeralRuins
drownedAcropolisEphemeralRuins = location DrownedAcropolisEphemeralRuins Cards.drownedAcropolisEphemeralRuins 3 (Static 2)

instance HasAbilities DrownedAcropolisEphemeralRuins where
  getAbilities (DrownedAcropolisEphemeralRuins a) =
    if a.unrevealed
      then extendUnrevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)
      else
        extendRevealed
          a
          [ restricted a 2 Here $ forced $ TurnEnds #after You
          , limitedAbility (GroupLimit PerGame 2) $ restricted a 3 Here actionAbility
          ]

instance RunMessage DrownedAcropolisEphemeralRuins where
  runMessage msg l@(DrownedAcropolisEphemeralRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      increaseThisFloodLevel attrs
      adjacent <- select $ connectedTo (be attrs) <> CanHaveFloodLevelIncreased
      chooseTargetM iid adjacent increaseThisFloodLevel
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseOrRunOneM iid $ withI18n do
        labeled' "increaseFloodLevelOfYourLocation" $ increaseThisFloodLevel attrs
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 2) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      increaseThisFloodLevel attrs
      flooded <- select FloodedLocation
      chooseTargetM iid flooded decreaseThisFloodLevel
      pure l
    _ -> DrownedAcropolisEphemeralRuins <$> liftRunMessage msg attrs
