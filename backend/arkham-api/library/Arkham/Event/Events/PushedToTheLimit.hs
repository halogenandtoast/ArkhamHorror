module Arkham.Event.Events.PushedToTheLimit (pushedToTheLimit) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getCanPerformAbility)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype PushedToTheLimit = PushedToTheLimit EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedToTheLimit :: EventCard PushedToTheLimit
pushedToTheLimit = event PushedToTheLimit Cards.pushedToTheLimit

instance RunMessage PushedToTheLimit where
  runMessage msg e@(PushedToTheLimit attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <-
        select
          $ oneOf [#tool, #weapon]
          <> #asset
          <> inDiscardOf iid
          <> CardWithPerformableAbility AbilityIsActionAbility [IgnoreAllCosts]
      focusCards cards do
        chooseOneM iid do
          targets cards \card -> do
            unfocusCards
            push $ AddCardEntity card
            handleTarget iid attrs (AssetId $ unsafeCardIdToUUID card.id)
            push $ RemoveCardEntity card
            shuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let
        adjustAbility ab =
          applyAbilityModifiers
            (ab {abilityDoesNotProvokeAttacksOfOpportunity = True})
            [IgnoreAllCosts]
      abilities <-
        selectMap adjustAbility
          $ AssetAbility (AssetWithId aid)
          <> AbilityIsActionAbility
      abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) abilities
      chooseOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities']
      pure e
    _ -> PushedToTheLimit <$> liftRunMessage msg attrs
