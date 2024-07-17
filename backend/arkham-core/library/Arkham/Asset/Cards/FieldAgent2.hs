module Arkham.Asset.Cards.FieldAgent2 (FieldAgent2 (..), fieldAgent2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Modifier

newtype FieldAgent2 = FieldAgent2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fieldAgent2 :: AssetCard FieldAgent2
fieldAgent2 = ally FieldAgent2 Cards.fieldAgent2 (2, 2)

instance HasModifiersFor FieldAgent2 where
  getModifiersFor (InvestigatorTarget iid) (FieldAgent2 a) | controlledBy a iid = do
    modified a [SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities FieldAgent2 where
  getAbilities (FieldAgent2 x) =
    [ controlledAbility x 1 (CanDiscoverCluesAt YourLocation) $ FastAbility (exhaust x <> horrorCost x 1)
    ]

instance RunMessage FieldAgent2 where
  runMessage msg a@(FieldAgent2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> FieldAgent2 <$> liftRunMessage msg attrs
