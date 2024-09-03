module Arkham.Asset.Cards.DisciplineQuiescenceOfThoughtBroken (
  disciplineQuiescenceOfThoughtBroken,
  DisciplineQuiescenceOfThoughtBroken (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier

newtype DisciplineQuiescenceOfThoughtBroken = DisciplineQuiescenceOfThoughtBroken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineQuiescenceOfThoughtBroken :: AssetCard DisciplineQuiescenceOfThoughtBroken
disciplineQuiescenceOfThoughtBroken = asset DisciplineQuiescenceOfThoughtBroken Cards.disciplineQuiescenceOfThoughtBroken

instance HasAbilities DisciplineQuiescenceOfThoughtBroken where
  getAbilities (DisciplineQuiescenceOfThoughtBroken x) =
    [ controlledAbility
        x
        1
        (youExist (InvestigatorWithMetaKey "quiescent") <> not_ (SelfHasModifier CannotBeFlipped))
        $ ReactionAbility (RoundEnds #after) Free
    ]

instance RunMessage DisciplineQuiescenceOfThoughtBroken where
  runMessage msg a@(DisciplineQuiescenceOfThoughtBroken attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineQuiescenceOfThoughtBroken <$> liftRunMessage msg attrs
