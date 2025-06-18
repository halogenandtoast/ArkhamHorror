module Arkham.Asset.Assets.ClaireWilson (claireWilson) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype ClaireWilson = ClaireWilson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

claireWilson :: AssetCard ClaireWilson
claireWilson = allyWith ClaireWilson Cards.claireWilson (2, 2) noSlots

instance HasAbilities ClaireWilson where
  getAbilities (ClaireWilson a) =
    [ controlled a 1 (DuringSkillTest $ YourSkillTest AnySkillTest)
        $ triggered (CommittedCards #after You $ LengthIs $ atLeast 1) (exhaust a)
    ]

instance RunMessage ClaireWilson where
  runMessage msg a@(ClaireWilson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    Flip _ ScenarioSource (isTarget attrs -> True) -> do
      pure $ ClaireWilson $ attrs & flippedL .~ True & visibleL .~ False
    Flip _ _ (isTarget attrs -> True) -> do
      let flipped = not $ view flippedL attrs
      pure $ ClaireWilson $ attrs & flippedL .~ flipped & visibleL .~ True
    _ -> ClaireWilson <$> liftRunMessage msg attrs
