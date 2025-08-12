module Arkham.Asset.Assets.EonChart4 (eonChart4) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Taboo
import Arkham.Window (defaultWindows)

newtype EonChart4 = EonChart4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eonChart4 :: AssetCard EonChart4
eonChart4 = asset EonChart4 Cards.eonChart4

instance HasAbilities EonChart4 where
  getAbilities (EonChart4 attrs) =
    [controlled attrs 1 matcher $ FastAbility (exhaust attrs <> assetUseCost attrs Secret 1)]
   where
    actions = [#move, #investigate, #evade]
    matcher =
      oneOf
        [ exists $ PerformableAbility [ActionCostModifier (-1)] <> mapOneOf AbilityIsAction actions
        , PlayableCardExists (UnpaidCost NoAction) $ basic (mapOneOf CardWithAction actions)
        ]

getAvailable :: HasGame m => InvestigatorId -> AssetAttrs -> [Action] -> m ([Card], [Ability])
getAvailable iid attrs canDoActions = do
  playableCards <- if tabooed TabooList20 attrs
    then pure []
    else do
      cards <- filterCards (mapOneOf CardWithAction canDoActions) <$> field InvestigatorHand iid
      filterM (getIsPlayable iid (toSource attrs) (UnpaidCost NoAction) (defaultWindows iid)) cards

  let abilityF = if tabooed TabooList20 attrs then (<> BasicAbility) else id
  abilities <- select (abilityF $ PerformableAbility [ActionCostModifier (-1)] <> oneOf (map AbilityIsAction canDoActions))
  pure (playableCards, abilities)

getAvailableActionTypes :: HasGame m => InvestigatorId -> AssetAttrs -> [Action] -> m [Action]
getAvailableActionTypes iid attrs canDoActions = do
  (cards, abilities) <- getAvailable iid attrs canDoActions
  let cActions = nub $ concatMap (.actions) cards
  let aActions = nub $ concatMap abilityActions abilities
  let allActions = nub $ cActions <> aActions
  pure $ filter (`elem` allActions) canDoActions

handleAction :: ReverseQueue m => InvestigatorId -> AssetAttrs -> Action -> m ()
handleAction iid attrs action = do
  (cards, abilities) <- getAvailable iid attrs [action]
  chooseOneM iid do
    for_ abilities \ab -> do
      abilityLabeled iid (decrease_ ab 1) $ handleTarget iid attrs (AbilityTarget iid ab.ref)
    targets cards $ playCardPayingCost iid

chooseAction :: ReverseQueue m => InvestigatorId -> AssetAttrs -> Message -> [Action] -> m ()
chooseAction iid attrs msg canDoActions = do
  actions' <- getAvailableActionTypes iid attrs canDoActions
  chooseOrRunOneM iid do
    when (#move `elem` actions') $ labeled "Move" $ forAction #move msg
    when (#evade `elem` actions') $ labeled "Evade" $ forAction #evade msg
    when (#investigate `elem` actions') $ labeled "Investigate" $ forAction #investigate msg

instance RunMessage EonChart4 where
  runMessage msg a@(EonChart4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAction iid attrs msg [#move, #evade, #investigate]
      pure a
    ForAction action (UseThisAbility iid (isSource attrs -> True) 1) -> do
      handleAction iid attrs action
      doStep 2 msg
      pure a
    DoStep 2 (ForAction action (UseThisAbility iid (isSource attrs -> True) 1)) -> do
      chooseAction iid attrs msg $ filter (/= action) [#move, #evade, #investigate]
      pure a
    ForAction action (DoStep 2 (ForAction _ (UseThisAbility iid (isSource attrs -> True) 1))) -> do
      handleAction iid attrs action
      pure a
    _ -> EonChart4 <$> liftRunMessage msg attrs
