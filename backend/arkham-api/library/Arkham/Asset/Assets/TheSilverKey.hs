module Arkham.Asset.Assets.TheSilverKey (theSilverKey) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message (cancelHorror)
import Arkham.Matcher

newtype TheSilverKey = TheSilverKey AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilverKey :: AssetCard TheSilverKey
theSilverKey = asset TheSilverKey Cards.theSilverKey

instance HasAbilities TheSilverKey where
  getAbilities (TheSilverKey a) =
    [ controlled_ a 1
        $ triggered (InvestigatorWouldTakeHorror #when You (SourceIsCancelable AnySource)) (exhaust a)
    ]

instance RunMessage TheSilverKey where
  runMessage msg a@(TheSilverKey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelHorror iid (attrs.ability 1) 1
      pure a
    _ -> TheSilverKey <$> liftRunMessage msg attrs
