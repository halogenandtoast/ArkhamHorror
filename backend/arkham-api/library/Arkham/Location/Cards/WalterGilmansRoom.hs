module Arkham.Location.Cards.WalterGilmansRoom (walterGilmansRoom) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WalterGilmansRoom = WalterGilmansRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

walterGilmansRoom :: LocationCard WalterGilmansRoom
walterGilmansRoom =
  locationWith WalterGilmansRoom Cards.walterGilmansRoom 4 (PerPlayer 1)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 1) (locationIs Locations.moldyHalls)

instance HasAbilities WalterGilmansRoom where
  getAbilities (WalterGilmansRoom a) =
    extendRevealed
      a
      [ restricted a 1 Here actionAbility
      , withI18n $ countVar 2 $ hauntedI "discardTopOfEncounterDeck" a 2
      ]

instance RunMessage WalterGilmansRoom where
  runMessage msg l@(WalterGilmansRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 3
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfEncounterDeck iid (attrs.ability 2) 2
      pure l
    _ -> WalterGilmansRoom <$> liftRunMessage msg attrs
