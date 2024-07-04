module Arkham.Asset.Cards.DisciplinePrescienceOfFateBroken (
  disciplinePrescienceOfFateBroken,
  DisciplinePrescienceOfFateBroken (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier

newtype DisciplinePrescienceOfFateBroken = DisciplinePrescienceOfFateBroken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplinePrescienceOfFateBroken :: AssetCard DisciplinePrescienceOfFateBroken
disciplinePrescienceOfFateBroken = asset DisciplinePrescienceOfFateBroken Cards.disciplinePrescienceOfFateBroken

instance HasAbilities DisciplinePrescienceOfFateBroken where
  getAbilities (DisciplinePrescienceOfFateBroken x) =
    [ controlledAbility
        x
        1
        (youExist (InvestigatorWithMetaKey "prescient") <> not_ (SelfHasModifier CannotBeFlipped))
        $ ReactionAbility (RoundEnds #after) Free
    ]

instance RunMessage DisciplinePrescienceOfFateBroken where
  runMessage msg a@(DisciplinePrescienceOfFateBroken attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplinePrescienceOfFateBroken <$> liftRunMessage msg attrs
