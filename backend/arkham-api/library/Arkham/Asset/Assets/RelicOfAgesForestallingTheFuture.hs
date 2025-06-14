module Arkham.Asset.Assets.RelicOfAgesForestallingTheFuture (relicOfAgesForestallingTheFuture) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Metadata = Metadata {successTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RelicOfAgesForestallingTheFuture = RelicOfAgesForestallingTheFuture (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesForestallingTheFuture :: AssetCard RelicOfAgesForestallingTheFuture
relicOfAgesForestallingTheFuture =
  asset
    (RelicOfAgesForestallingTheFuture . (`with` Metadata False))
    Cards.relicOfAgesForestallingTheFuture

instance HasAbilities RelicOfAgesForestallingTheFuture where
  getAbilities (RelicOfAgesForestallingTheFuture (a `With` _)) =
    [skillTestAbility $ restricted a 1 ControlsThis $ FastAbility $ exhaust a]

instance RunMessage RelicOfAgesForestallingTheFuture where
  runMessage msg a@(RelicOfAgesForestallingTheFuture (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind $ beginSkillTest sid iid attrs iid kind (Fixed 4)
      pure a
    PassedThisSkillTest _ (isSource attrs -> True) | not (successTriggered metadata) -> do
      agenda <- selectJust AnyAgenda
      removeDoom (attrs.ability 1) agenda 1
      pure . RelicOfAgesForestallingTheFuture $ attrs `with` Metadata True
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      placeDoom (attrs.ability 1) attrs 1
      pure a
    _ -> RelicOfAgesForestallingTheFuture . (`with` metadata) <$> liftRunMessage msg attrs
