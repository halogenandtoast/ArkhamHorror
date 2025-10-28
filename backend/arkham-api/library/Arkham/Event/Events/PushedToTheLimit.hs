module Arkham.Event.Events.PushedToTheLimit (pushedToTheLimit) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Ability (getCanPerformAbility)
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
          <> CardWithPerformableAbility #action [IgnoreAllCosts]
      focusCards cards do
        chooseOneM iid do
          targets cards \card -> do
            unfocusCards
            withCardEntity @AssetId card $ handleTarget iid attrs
            shuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      let adjustAbility ab = applyAbilityModifiers (noAOO ab) [IgnoreAllCosts]
      abilities <- selectMap adjustAbility $ AssetAbility (AssetWithId aid) <> #action
      abilities' <- filterM (getCanPerformAbility iid (defaultWindows iid)) abilities
      chooseOne iid [AbilityLabel iid ab [] [] [] | ab <- abilities']
      pure e
    _ -> PushedToTheLimit <$> liftRunMessage msg attrs
