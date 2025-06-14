module Arkham.Location.Cards.ReturnToMetropolitanCathedral (returnToMetropolitanCathedral) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToMetropolitanCathedral = ReturnToMetropolitanCathedral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToMetropolitanCathedral :: LocationCard ReturnToMetropolitanCathedral
returnToMetropolitanCathedral =
  symbolLabel
    $ location ReturnToMetropolitanCathedral Cards.returnToMetropolitanCathedral 4 (Static 0)

instance HasAbilities ReturnToMetropolitanCathedral where
  getAbilities (ReturnToMetropolitanCathedral a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> youExist can.gain.resources)
          $ actionAbilityWithCost (HorrorCost (a.ability 1) YouTarget 1)
      , restricted a 2 (Here <> youExist (InvestigatorWithResources $ atLeast 10)) exploreAction_
      ]

instance RunMessage ReturnToMetropolitanCathedral where
  runMessage msg l@(ReturnToMetropolitanCathedral attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToMetropolitanCathedral <$> liftRunMessage msg attrs
