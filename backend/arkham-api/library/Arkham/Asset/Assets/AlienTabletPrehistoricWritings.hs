module Arkham.Asset.Assets.AlienTabletPrehistoricWritings (alienTablet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, withSkillTest)
import Arkham.Matcher
import Arkham.Trait (Trait (Glyph, Rlyeh))

newtype AlienTabletPrehistoricWritings = AlienTabletPrehistoricWritings AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienTablet :: AssetCard AlienTabletPrehistoricWritings
alienTablet = asset AlienTabletPrehistoricWritings Cards.alienTablet

instance HasAbilities AlienTabletPrehistoricWritings where
  getAbilities (AlienTabletPrehistoricWritings a) =
    [ controlledAbility
        a
        1
        (DuringSkillTest $ mapOneOf SkillTestOnCardWithTrait [Glyph, Rlyeh])
        (FastAbility $ exhaust a <> assetUseCost a Secret 1)
    ]

instance RunMessage AlienTabletPrehistoricWritings where
  runMessage msg a@(AlienTabletPrehistoricWritings attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        whenJustM getSkillTestInvestigator \iid ->
          modifyAnySkill sid (attrs.ability 1) iid 2
      pure a
    _ -> AlienTabletPrehistoricWritings <$> liftRunMessage msg attrs
