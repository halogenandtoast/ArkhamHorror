module Arkham.Location.Cards.ATearInThePath (aTearInThePath) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (aTearInThePath)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ATearInThePath = ATearInThePath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInThePath :: LocationCard ATearInThePath
aTearInThePath = location ATearInThePath Cards.aTearInThePath 3 (PerPlayer 1)

instance HasAbilities ATearInThePath where
  getAbilities (ATearInThePath attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 (youExist InvestigatorWithoutActionsRemaining)
      $ forced
      $ RevealLocation #after You (be attrs)

instance RunMessage ATearInThePath where
  runMessage msg l@(ATearInThePath attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 2
      pure l
    _ -> ATearInThePath <$> liftRunMessage msg attrs
