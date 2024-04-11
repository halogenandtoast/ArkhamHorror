module Arkham.Asset.Cards.DisciplineBalanceOfBodyBroken (
  disciplineBalanceOfBodyBroken,
  DisciplineBalanceOfBodyBroken (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier

newtype DisciplineBalanceOfBodyBroken = DisciplineBalanceOfBodyBroken AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineBalanceOfBodyBroken :: AssetCard DisciplineBalanceOfBodyBroken
disciplineBalanceOfBodyBroken = asset DisciplineBalanceOfBodyBroken Cards.disciplineBalanceOfBodyBroken

instance HasAbilities DisciplineBalanceOfBodyBroken where
  getAbilities (DisciplineBalanceOfBodyBroken x) =
    [ controlledAbility
        x
        1
        (youExist (InvestigatorWithMetaKey "balanced") <> not_ (SelfHasModifier CannotBeFlipped))
        $ ReactionAbility (RoundEnds #after) Free
    ]

instance RunMessage DisciplineBalanceOfBodyBroken where
  runMessage msg a@(DisciplineBalanceOfBodyBroken attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineBalanceOfBodyBroken <$> lift (runMessage msg attrs)
