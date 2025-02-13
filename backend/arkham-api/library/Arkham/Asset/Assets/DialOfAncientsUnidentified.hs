module Arkham.Asset.Assets.DialOfAncientsUnidentified (dialOfAncientsUnidentified) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.CampaignLogKey
import Arkham.Matcher

newtype DialOfAncientsUnidentified = DialOfAncientsUnidentified AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dialOfAncientsUnidentified :: AssetCard DialOfAncientsUnidentified
dialOfAncientsUnidentified = asset DialOfAncientsUnidentified Cards.dialOfAncientsUnidentified

instance HasAbilities DialOfAncientsUnidentified where
  getAbilities (DialOfAncientsUnidentified attrs) =
    [restricted attrs 1 ControlsThis $ freeReaction (EnemyEvaded #after Anyone (at_ YourLocation))]

instance RunMessage DialOfAncientsUnidentified where
  runMessage msg a@(DialOfAncientsUnidentified attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addUses (attrs.ability 1) attrs Charge 1
      doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      recordWhen (attrs.use Charge >= 4) YouHaveCalculatedTheDayOfReckoning
      pure a
    _ -> DialOfAncientsUnidentified <$> liftRunMessage msg attrs
