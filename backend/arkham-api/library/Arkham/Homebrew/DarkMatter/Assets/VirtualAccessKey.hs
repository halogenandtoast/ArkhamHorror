module Arkham.Homebrew.DarkMatter.Assets.VirtualAccessKey (virtualAccessKey) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message (cancelHorror)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype VirtualAccessKey = VirtualAccessKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virtualAccessKey :: AssetCard VirtualAccessKey
virtualAccessKey = asset VirtualAccessKey Cards.virtualAccessKey

instance HasAbilities VirtualAccessKey where
  getAbilities (VirtualAccessKey a) =
    [ controlled_ a 1
        $ triggered (InvestigatorWouldTakeHorror #when You (SourceIsCancelable AnySource)) (exhaust a)
    ]

instance RunMessage VirtualAccessKey where
  runMessage msg a@(VirtualAccessKey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelHorror iid (attrs.ability 1) 1
      pure a
    _ -> VirtualAccessKey <$> liftRunMessage msg attrs
