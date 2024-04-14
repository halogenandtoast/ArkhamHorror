module Arkham.Asset.Cards.Grounded3 (grounded3, Grounded3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Types (sanityL)
import Arkham.Matcher
import Arkham.Modifier

newtype Grounded3 = Grounded3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grounded3 :: AssetCard Grounded3
grounded3 = assetWith Grounded3 Cards.grounded3 $ (healthL ?~ 2) . (sanityL ?~ 2)

instance HasAbilities Grounded3 where
  getAbilities (Grounded3 x) =
    [ controlledAbility x 1 (DuringSkillTest AnySkillTest)
        $ FastAbility (ResourceCost 1)
    ]

instance HasModifiersFor Grounded3 where
  getModifiersFor target (Grounded3 attrs) | attrs `is` target = do
    pure
      $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst, NonDirectDamageMustBeAssignToThisFirst]
  getModifiersFor (InvestigatorTarget iid) (Grounded3 attrs) | iid `controls` attrs = do
    pure $ toModifiers attrs [AnySkillValue 1]
  getModifiersFor _ _ = pure []

instance RunMessage Grounded3 where
  runMessage msg a@(Grounded3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifiers (attrs.ability 1) iid [AnySkillValue 1]
      pure a
    _ -> Grounded3 <$> lift (runMessage msg attrs)
