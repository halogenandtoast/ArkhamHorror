module Arkham.Asset.Assets.ClaireWilson (
  claireWilson,
  ClaireWilson(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype ClaireWilson = ClaireWilson AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

claireWilson :: AssetCard ClaireWilson
claireWilson = allyWith ClaireWilson Cards.claireWilson (2, 2) noSlots

instance HasAbilities ClaireWilson where
  getAbilities (ClaireWilson a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest (YourSkillTest AnySkillTest))
        $ ReactionAbility (CommittedCards #after You $ LengthIs $ AtLeast $ Static 1) (exhaust a)
    ]

instance RunMessage ClaireWilson where
  runMessage msg a@(ClaireWilson attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      pure a
    _ -> ClaireWilson <$> liftRunMessage msg attrs
