module Arkham.Asset.Cards.DisciplineBalanceOfBody (
  disciplineBalanceOfBody,
  DisciplineBalanceOfBody (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Modifier

newtype Metadata = Metadata {chosenAbilities :: [DifferentAbility]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DisciplineBalanceOfBody = DisciplineBalanceOfBody (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineBalanceOfBody :: AssetCard DisciplineBalanceOfBody
disciplineBalanceOfBody = asset (DisciplineBalanceOfBody . (`with` Metadata [])) Cards.disciplineBalanceOfBody

instance HasModifiersFor DisciplineBalanceOfBody where
  getModifiersFor (InvestigatorTarget iid) (DisciplineBalanceOfBody (With a _)) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DisciplineBalanceOfBody where
  getAbilities (DisciplineBalanceOfBody (With x _)) =
    [ doesNotProvokeAttacksOfOpportunity
        $ restrictedAbility x 1 ControlsThis actionAbility
    ]

instance RunMessage DisciplineBalanceOfBody where
  runMessage msg a@(DisciplineBalanceOfBody (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DoStep 3 msg
      flipOverBy iid (attrs.ability 1) attrs
      pure . DisciplineBalanceOfBody $ attrs `with` Metadata []
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      abilities' <-
        selectMap DifferentAbility
          $ PerformableAbility [ActionCostModifier (-1)]
          <> oneOf [AbilityIsAction #fight, AbilityIsAction #evade]
      chooseOrRunOne iid $ Label "Take no more actions" []
        : [ AbilityLabel
            iid
            ab
            []
            [HandleTargetChoice iid (toSource attrs) (AbilityTarget iid ab), DoStep (n - 1) msg']
          | DifferentAbility ab <- filter (`notElem` chosenAbilities meta) abilities'
          ]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (AbilityTarget _ ab) -> do
      pure . DisciplineBalanceOfBody $ attrs `with` Metadata (DifferentAbility ab : chosenAbilities meta)
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineBalanceOfBody . (`with` meta) <$> lift (runMessage msg attrs)
