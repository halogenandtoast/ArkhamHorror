module Arkham.Asset.Assets.DisciplineBalanceOfBody (
  disciplineBalanceOfBody,
  DisciplineBalanceOfBody (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Game.Helpers (canDo, getPlayableCards)
import Arkham.Helpers.Modifiers (modified_, withGrantedAction)
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype Metadata = Metadata {chosenAbilities :: [DifferentAbility]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DisciplineBalanceOfBody = DisciplineBalanceOfBody (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disciplineBalanceOfBody :: AssetCard DisciplineBalanceOfBody
disciplineBalanceOfBody = asset (DisciplineBalanceOfBody . (`with` Metadata [])) Cards.disciplineBalanceOfBody

instance HasModifiersFor DisciplineBalanceOfBody where
  getModifiersFor (DisciplineBalanceOfBody (With a _)) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modified_ a iid [SkillModifier #agility 1]

instance HasAbilities DisciplineBalanceOfBody where
  getAbilities (DisciplineBalanceOfBody (With x _)) =
    [doesNotProvokeAttacksOfOpportunity $ restricted x 1 ControlsThis actionAbility]

instance RunMessage DisciplineBalanceOfBody where
  runMessage msg a@(DisciplineBalanceOfBody (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      doStep 3 msg
      flipOverBy iid (attrs.ability 1) attrs
      pure . DisciplineBalanceOfBody $ attrs `with` Metadata []
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      abilities' <-
        selectMap DifferentAbility
          $ PerformableAbility [ActionCostModifier (-1)]
          <> oneOf [AbilityIsAction #fight, AbilityIsAction #evade]

      iattrs <- getAttrs @Investigator iid
      playableCards <- withGrantedAction iid attrs do
        filterCards (mapOneOf CardWithAction [#fight, #evade])
          <$> getPlayableCards iattrs (UnpaidCost NoAction) (defaultWindows iid)

      chooseOrRunOneM iid do
        labeled "Take no more actions" nothing
        for_ (filter (`notElem` chosenAbilities meta) abilities') \(DifferentAbility ab) -> do
          abilityLabeled iid (overCost (`decreaseActionCost` 1) ab) do
            handleTarget iid (toSource attrs) (AbilityTarget iid ab)
            doStep (n - 1) msg'
        whenM (canDo iid #play) do
          targets playableCards \c -> do
            playCardPayingCost iid c
            doStep (n - 1) msg'
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (AbilityTarget _ ab) -> do
      pure . DisciplineBalanceOfBody $ attrs `with` Metadata (DifferentAbility ab : chosenAbilities meta)
    Flip iid _ (isTarget attrs -> True) -> do
      push $ ReplaceInvestigatorAsset iid attrs.id (flipCard $ toCard attrs)
      pure a
    _ -> DisciplineBalanceOfBody . (`with` meta) <$> liftRunMessage msg attrs
