module Arkham.Location.Cards.BahiaPalaceGardens (bahiaPalaceGardens) where

import Arkham.Ability
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Helpers.Location (swapLocation)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.DeadHeat.Helpers

newtype BahiaPalaceGardens = BahiaPalaceGardens LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bahiaPalaceGardens :: LocationCard BahiaPalaceGardens
bahiaPalaceGardens = symbolLabel $ location BahiaPalaceGardens Cards.bahiaPalaceGardens 4 (PerPlayer 2)

instance HasAbilities BahiaPalaceGardens where
  getAbilities (BahiaPalaceGardens a) =
    if a.revealed
      then
        extendRevealed
          a
          [ restricted a 1 (Here <> youExist (oneOf [InvestigatorWithAnyHorror, InvestigatorWithAnyDamage]))
              $ freeReaction
              $ ScenarioEvent #after (Just You) "rescueCivilian[09530]"
          , becomeAbandonedAbility a 2
          ]
      else extendUnrevealed1 a $ becomeAbandonedAbility a 1

instance RunMessage BahiaPalaceGardens where
  runMessage msg l@(BahiaPalaceGardens attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 | attrs.unrevealed -> do
      swapLocation attrs =<< fetchCard Cards.bahiaPalaceGardensAbandoned
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      damageOk <- canHaveDamageHealed (attrs.ability 1) iid
      horrorOk <- canHaveHorrorHealed (attrs.ability 1) iid
      chooseOrRunOneM iid $ withI18n do
        when damageOk $ countVar 1 $ labeled' "healDamage" $ healDamage iid (attrs.ability 1) 1
        when horrorOk $ countVar 1 $ labeled' "healHorror" $ healHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      swapLocation attrs =<< fetchCard Cards.bahiaPalaceGardensAbandoned
      pure l
    _ -> BahiaPalaceGardens <$> liftRunMessage msg attrs
