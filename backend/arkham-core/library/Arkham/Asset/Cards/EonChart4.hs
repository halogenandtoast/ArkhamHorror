module Arkham.Asset.Cards.EonChart4 (eonChart4, EonChart4 (..)) where

import Arkham.Ability
import Arkham.Action (Action)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype EonChart4 = EonChart4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eonChart4 :: AssetCard EonChart4
eonChart4 = asset EonChart4 Cards.eonChart4

instance HasAbilities EonChart4 where
  getAbilities (EonChart4 attrs) =
    [ controlledAbility
        attrs
        1
        ( oneOf
            [ exists
                $ PerformableAbility [ActionCostModifier (-1)]
                <> oneOf (map AbilityIsAction [#move, #investigate, #evade])
            , PlayableCardExists
                (UnpaidCost NoAction)
                (basic $ oneOf $ map CardWithAction [#move, #investigate, #evade])
            ]
        )
        $ FastAbility (exhaust attrs <> assetUseCost attrs Secret 1)
    ]

getAvailable :: HasGame m => InvestigatorId -> AssetAttrs -> [Action] -> m ([Card], [Ability])
getAvailable iid attrs canDoActions = do
  let windows' = defaultWindows iid
  handCards <- field InvestigatorHand iid
  let cards = filter (any (`elem` canDoActions) . cdActions . toCardDef) handCards
  playableCards <- filterM (getIsPlayable iid (toSource attrs) (UnpaidCost NoAction) windows') cards

  abilities' <-
    select (PerformableAbility [ActionCostModifier (-1)] <> oneOf (map AbilityIsAction canDoActions))
  pure (playableCards, abilities')

getAvailableActionTypes :: HasGame m => InvestigatorId -> AssetAttrs -> [Action] -> m [Action]
getAvailableActionTypes iid attrs canDoActions = do
  (cards, abilities) <- getAvailable iid attrs canDoActions
  let cActions = nub $ concatMap (cdActions . toCardDef) cards
  let aActions = nub $ concatMap abilityActions abilities
  let allActions = nub $ cActions <> aActions
  pure $ filter (`elem` allActions) canDoActions

instance RunMessage EonChart4 where
  runMessage msg a@(EonChart4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      actions <- getAvailableActionTypes iid attrs [#move, #evade, #investigate]
      chooseOrRunOne iid
        $ [Label "Move" [DoStep 0 msg] | #move `elem` actions]
        <> [Label "Evade" [DoStep 1 msg] | #evade `elem` actions]
        <> [Label "Investigate" [DoStep 2 msg] | #investigate `elem` actions]
      pure a
    DoStep n msg'@(UseCardAbility iid (isSource attrs -> True) 1 windows' _) -> do
      let
        action =
          case n `mod` 3 of
            0 -> #move
            1 -> #evade
            2 -> #investigate
            _ -> error "invalid action"
        canDoActions = filter (/= action) [#move, #evade, #investigate]

      (cards, abilities) <- getAvailable iid attrs [action]
      actions <- if n < 3 then getAvailableActionTypes iid attrs canDoActions else pure []

      let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]

      unless (null abilities && null cards) do
        chooseOne iid
          $ [ AbilityLabel
              iid
              ab
              []
              [HandleTargetChoice iid (toSource attrs) (AbilityTarget iid $ decreaseCost ab), DoStep (n - 1) msg']
            | ab <- abilities
            ]
          <> [targetLabel (toCardId item) [PayCardCost iid item windows'] | item <- cards]

      chooseOrRunOne iid
        $ [Label "Move" [DoStep 3 msg] | #move `elem` actions]
        <> [Label "Evade" [DoStep 4 msg] | #evade `elem` actions]
        <> [Label "Investigate" [DoStep 5 msg] | #investigate `elem` actions]
      pure a
    _ -> EonChart4 <$> liftRunMessage msg attrs
