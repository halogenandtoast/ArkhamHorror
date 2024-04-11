module Arkham.Asset.Cards.DisciplineAlignmentOfSpiritBroken (
  disciplineAlignmentOfSpiritBroken,
  DisciplineAlignmentOfSpiritBroken (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier

newtype DisciplineAlignmentOfSpiritBroken = DisciplineAlignmentOfSpiritBroken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineAlignmentOfSpiritBroken :: AssetCard DisciplineAlignmentOfSpiritBroken
disciplineAlignmentOfSpiritBroken = asset DisciplineAlignmentOfSpiritBroken Cards.disciplineAlignmentOfSpiritBroken

instance HasAbilities DisciplineAlignmentOfSpiritBroken where
  getAbilities (DisciplineAlignmentOfSpiritBroken x) =
    [ controlledAbility
        x
        1
        (youExist (InvestigatorWithMetaKey "aligned") <> not_ (SelfHasModifier CannotBeFlipped))
        $ ReactionAbility (RoundEnds #after) Free
    ]

instance RunMessage DisciplineAlignmentOfSpiritBroken where
  runMessage msg a@(DisciplineAlignmentOfSpiritBroken attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineAlignmentOfSpiritBroken <$> lift (runMessage msg attrs)
