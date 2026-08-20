module Arkham.Location.Cards.TheForgottenAge.ThreadsOfFate.ArkhamPoliceStation (arkhamPoliceStation) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Location.CardDefs.NightOfTheZealot.TheMidnightMasks qualified as Cards
import Arkham.Location.CardDefs.TheForgottenAge.ThreadsOfFate qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype ArkhamPoliceStation = ArkhamPoliceStation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamPoliceStation :: LocationCard ArkhamPoliceStation
arkhamPoliceStation = location ArkhamPoliceStation Cards.arkhamPoliceStation 3 (PerPlayer 2)

instance HasModifiersFor ArkhamPoliceStation where
  getModifiersFor (ArkhamPoliceStation a) = do
    modifySelectMap a (locationIs Cards.easttown) \lid -> [ConnectedToWhen (LocationWithId lid) (be a)]

instance HasAbilities ArkhamPoliceStation where
  getAbilities (ArkhamPoliceStation attrs) = extendRevealed attrs [restricted attrs 1 Here actionAbility]

instance RunMessage ArkhamPoliceStation where
  runMessage msg l@(ArkhamPoliceStation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] #weapon (AddFoundToHand iid 1)
      pure l
    _ -> ArkhamPoliceStation <$> liftRunMessage msg attrs
