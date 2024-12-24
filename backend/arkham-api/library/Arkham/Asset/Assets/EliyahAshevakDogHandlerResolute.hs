module Arkham.Asset.Assets.EliyahAshevakDogHandlerResolute (eliyahAshevakDogHandlerResolute) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Game.Helpers (getAccessibleLocations)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier

newtype EliyahAshevakDogHandlerResolute = EliyahAshevakDogHandlerResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eliyahAshevakDogHandlerResolute :: AssetCard EliyahAshevakDogHandlerResolute
eliyahAshevakDogHandlerResolute = allyWith EliyahAshevakDogHandlerResolute Cards.eliyahAshevakDogHandlerResolute (4, 4) noSlots

instance HasAbilities EliyahAshevakDogHandlerResolute where
  getAbilities (EliyahAshevakDogHandlerResolute a) =
    [ restricted a 1 (ControlsThis <> DuringTurn You)
        $ FastAbility' (assetUseCost a Secret 1 <> exhaust a) [#evade]
    ]

instance RunMessage EliyahAshevakDogHandlerResolute where
  runMessage msg a@(EliyahAshevakDogHandlerResolute attrs) = runQueueT $ case msg of
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
    _ -> EliyahAshevakDogHandlerResolute <$> liftRunMessage msg attrs
