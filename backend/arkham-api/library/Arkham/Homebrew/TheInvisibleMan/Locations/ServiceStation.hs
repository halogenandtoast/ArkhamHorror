{- | Service Station -- the attendant talks for money.

The group pays, so the cost is a 'GroupResourceCost' over the investigators
here rather than a cost on the one who triggers it.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.ServiceStation (serviceStation) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ServiceStation = ServiceStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serviceStation :: LocationCard ServiceStation
serviceStation = location ServiceStation Cards.serviceStation 3 (PerPlayer 1)

instance HasAbilities ServiceStation where
  getAbilities (ServiceStation a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "serviceStation.parley"
      $ onlyOnce
      $ restricted a 1 Here
      $ FastAbility' (GroupResourceCost (Static 3) (be a)) #parley

instance RunMessage ServiceStation where
  runMessage msg l@(ServiceStation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeLead (attrs.ability 1) attrs
      pure l
    _ -> ServiceStation <$> liftRunMessage msg attrs
