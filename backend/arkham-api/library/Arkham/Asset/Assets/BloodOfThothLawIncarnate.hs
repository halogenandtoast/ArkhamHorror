module Arkham.Asset.Assets.BloodOfThothLawIncarnate (bloodOfThothLawIncarnate) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Taboo
import Arkham.Token

newtype BloodOfThothLawIncarnate = BloodOfThothLawIncarnate AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodOfThothLawIncarnate :: AssetCard BloodOfThothLawIncarnate
bloodOfThothLawIncarnate = asset BloodOfThothLawIncarnate Cards.bloodOfThothLawIncarnate

instance HasAbilities BloodOfThothLawIncarnate where
  getAbilities (BloodOfThothLawIncarnate a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (PlacedToken #after AnySource (TargetControlledBy You) Doom)
          (if tabooed TabooList24 a then Free else exhaust a)
    , if tabooed TabooList24 a
        then
          controlled a 2 (DuringPhase #investigation <> not_ DuringAction)
            $ FastAbility (exhaust a <> assetUseCost a Offering 2)
        else controlled a 2 criteria $ FastAbility (exhaust a <> AllUsesCost (be a) Offering)
    ]
   where
    criteria = if a.use Offering >= 3 then NoRestriction else Never

instance RunMessage BloodOfThothLawIncarnate where
  runMessage msg a@(BloodOfThothLawIncarnate attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Offering 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <- select $ affectsOthers $ colocatedWith iid
      chooseTargetM iid iids (`takeActionAsIfTurn` attrs.ability 2)
      pure a
    _ -> BloodOfThothLawIncarnate <$> liftRunMessage msg attrs
