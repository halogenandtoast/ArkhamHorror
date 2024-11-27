module Arkham.Asset.Assets.EliyahAshevakDogHandler (
  eliyahAshevakDogHandler,
  EliyahAshevakDogHandler (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype EliyahAshevakDogHandler = EliyahAshevakDogHandler AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliyahAshevakDogHandler :: AssetCard EliyahAshevakDogHandler
eliyahAshevakDogHandler = allyWith EliyahAshevakDogHandler Cards.eliyahAshevakDogHandler (3, 3) noSlots

instance HasAbilities EliyahAshevakDogHandler where
  getAbilities (EliyahAshevakDogHandler a) =
    [restricted a 1 ControlsThis $ evadeAction (assetUseCost a Secret 1 <> exhaust a)]

instance RunMessage EliyahAshevakDogHandler where
  runMessage msg a@(EliyahAshevakDogHandler attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #agility 6)
      chooseEvadeEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      connected <- getAccessibleLocations iid attrs
      chooseOrRunOneM iid do
        labeled "Do not move to a connecting location" nothing
        targets connected $ moveTo attrs iid
      pure a
    _ -> EliyahAshevakDogHandler <$> liftRunMessage msg attrs
