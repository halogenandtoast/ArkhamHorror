module Arkham.Asset.Cards.EonChart1 (eonChart1, EonChart1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Game.Helpers (getActionsWith, getIsPlayable)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype EonChart1 = EonChart1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eonChart1 :: AssetCard EonChart1
eonChart1 = asset EonChart1 Cards.eonChart1

instance HasAbilities EonChart1 where
  getAbilities (EonChart1 attrs) =
    [ controlledAbility
        attrs
        1
        ( oneOf
            [ exists $ oneOf [#move, #investigate, #evade] <> PerformableAbility [ActionCostModifier (-1)]
            , PlayableCardExists
                (UnpaidCost NoAction)
                (basic $ oneOf $ map CardWithAction [#move, #investigate, #evade])
            ]
        )
        $ FastAbility (exhaust attrs <> assetUseCost attrs Secret 1)
    ]

instance RunMessage EonChart1 where
  runMessage msg a@(EonChart1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let windows' = defaultWindows iid
      let decreaseCost = flip applyAbilityModifiers [ActionCostModifier (-1)]
      let canDoActions = [#move, #evade, #investigate]
      actions <- getActionsWith iid windows' decreaseCost
      handCards <- field InvestigatorHand iid
      let cards = filter (any (`elem` canDoActions) . cdActions . toCardDef) handCards
      playableCards <- filterM (getIsPlayable iid (toSource attrs) (UnpaidCost NoAction) windows') cards
      player <- getPlayer iid
      push
        $ AskPlayer
        $ Msg.chooseOne player
        $ map
          ((\f -> f windows' []) . AbilityLabel iid)
          (filter (\x -> any (abilityIs x) [#move, #evade, #investigate]) actions)
        <> [targetLabel (toCardId item) [PayCardCost iid item windows'] | item <- playableCards]
      pure a
    _ -> EonChart1 <$> lift (runMessage msg attrs)
