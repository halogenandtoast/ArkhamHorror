module Arkham.Location.Cards.ArkhamWarOfTheOuterGods (arkham) where

import Arkham.Ability
import Arkham.Helpers.Location (getAccessibleLocations)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype ArkhamWarOfTheOuterGods = ArkhamWarOfTheOuterGods LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkham :: LocationCard ArkhamWarOfTheOuterGods
arkham = symbolLabel $ location ArkhamWarOfTheOuterGods Cards.arkham 5 (PerPlayer 1)

instance HasModifiersFor ArkhamWarOfTheOuterGods where
  getModifiersFor (ArkhamWarOfTheOuterGods a) = modifySelf a [CannotHaveAttachments]

instance HasAbilities ArkhamWarOfTheOuterGods where
  getAbilities (ArkhamWarOfTheOuterGods a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ FastAbility
      $ GroupResourceCost (PerPlayer 1) YourLocation

instance RunMessage ArkhamWarOfTheOuterGods where
  runMessage msg l@(ArkhamWarOfTheOuterGods attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connecting <- getAccessibleLocations iid (attrs.ability 1)
      chooseTargetM iid connecting (moveTo (attrs.ability 1) iid)
      pure l
    _ -> ArkhamWarOfTheOuterGods <$> liftRunMessage msg attrs
