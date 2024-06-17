module Arkham.Asset.Cards.DisciplineQuiescenceOfThought (
  disciplineQuiescenceOfThought,
  DisciplineQuiescenceOfThought (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype DisciplineQuiescenceOfThought = DisciplineQuiescenceOfThought AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineQuiescenceOfThought :: AssetCard DisciplineQuiescenceOfThought
disciplineQuiescenceOfThought = asset DisciplineQuiescenceOfThought Cards.disciplineQuiescenceOfThought

instance HasModifiersFor DisciplineQuiescenceOfThought where
  getModifiersFor (InvestigatorTarget iid) (DisciplineQuiescenceOfThought a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DisciplineQuiescenceOfThought where
  getAbilities (DisciplineQuiescenceOfThought x) =
    [controlledAbility x 1 (youExist $ HandWith $ LengthIs $ lessThan 5) actionAbility]

instance RunMessage DisciplineQuiescenceOfThought where
  runMessage msg a@(DisciplineQuiescenceOfThought attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DoStep 1 msg
      flipOverBy iid (attrs.ability 1) attrs
      pure a
    DoStep 1 msg'@(UseThisAbility iid (isSource attrs -> True) 1) -> do
      finished <- fieldMap InvestigatorHand ((>= 5) . length) iid
      canDrawCards <- can.draw.cards iid
      when (not finished && canDrawCards) do
        let drawing = Msg.drawCards iid (attrs.ability 1) 1
        pushAll [drawing, DoStep 1 msg']
      pure a
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineQuiescenceOfThought <$> lift (runMessage msg attrs)
