module Arkham.Asset.Assets.KeyOfMysteries (keyOfMysteries) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Discover (insteadOfDiscoveringClues)
import Arkham.Message.Lifted.Placement

newtype KeyOfMysteries = KeyOfMysteries AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyOfMysteries :: AssetCard KeyOfMysteries
keyOfMysteries = asset KeyOfMysteries Cards.keyOfMysteries

instance HasAbilities KeyOfMysteries where
  getAbilities (KeyOfMysteries a) =
    [ restricted a 1 ControlsThis $ forced $ Matcher.InvestigatorDefeated #when ByAny You
    , investigateAbility a 2 mempty (Uncontrolled <> OnSameLocation)
    ]

instance RunMessage KeyOfMysteries where
  runMessage msg a@(KeyOfMysteries attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      getLocationOf iid >>= traverse_ \lid -> do
        loseControlOfAsset attrs.id
        place attrs (AtLocation lid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 2) (setTarget attrs)
      pure a
    SuccessfulInvestigationWith iid (isTarget attrs -> True) -> do
      insteadOfDiscoveringClues iid \_ -> pure ()
      takeControlOfAsset iid attrs.id
      pure a
    _ -> KeyOfMysteries <$> liftRunMessage msg attrs
