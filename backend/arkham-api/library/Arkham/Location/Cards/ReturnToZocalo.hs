module Arkham.Location.Cards.ReturnToZocalo (returnToZocalo) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToZocalo = ReturnToZocalo LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToZocalo :: LocationCard ReturnToZocalo
returnToZocalo = symbolLabel $ location ReturnToZocalo Cards.returnToZocalo 2 (Static 0)

instance HasAbilities ReturnToZocalo where
  getAbilities (ReturnToZocalo a) = extendRevealed1 a $ restricted a 1 Here $ exploreAction $ SkillIconCost 4 mempty

instance RunMessage ReturnToZocalo where
  runMessage msg l@(ReturnToZocalo attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ReturnToZocalo <$> liftRunMessage msg attrs
