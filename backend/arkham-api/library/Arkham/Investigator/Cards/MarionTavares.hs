module Arkham.Investigator.Cards.MarionTavares (marionTavares) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Draw.Types
import Arkham.Helpers.Playable (getPlayableCards)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (PlayCard)
import Arkham.Investigator.Projection ()
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Slot
import Arkham.Strategy

newtype MarionTavares = MarionTavares InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

setSlots :: InvestigatorAttrs -> InvestigatorAttrs
setSlots attrs = attrs & slotsL . at HandSlot ?~ [Slot (InvestigatorSource attrs.id) []]

marionTavares :: InvestigatorCard MarionTavares
marionTavares =
  investigatorWith
    MarionTavares
    Cards.marionTavares
    (Stats {health = 8, sanity = 6, willpower = 2, intellect = 3, combat = 4, agility = 3})
    setSlots

instance HasAbilities MarionTavares where
  getAbilities (MarionTavares a) =
    [ playerLimit PerRound
        $ selfAbility a 1 (DuringTurn You <> youExist can.draw.cards)
        $ freeReaction (PlayCard #after You (basic #event))
    ]

instance HasChaosTokenValue MarionTavares where
  getChaosTokenValue iid ElderSign (MarionTavares attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarionTavares where
  runMessage msg i@(MarionTavares attrs) = runQueueT $ case msg of
    ElderSignEffect iid | iid == attrs.id -> do
      search iid ElderSign iid [fromTopOfDeck 3] (basic #event) (DrawFound iid 1)
      pure i
    ResetGame -> MarionTavares . setSlots <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsEdit iid (attrs.ability 1) 1 \c -> c {cardDrawAndThen = Just (DoStep 1 msg)}
      pure i
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 ws@(cardPlayed -> card) _) -> do
      cards <-
        filterCards (#event <> not_ (CardWithTitle card.title))
          <$> getPlayableCards (attrs.ability 1) iid (UnpaidCost NoAction) ws
      when (notNull cards) do
        chooseOneM iid do
          labeled "Do not play another event" nothing
          targets cards (playCardPayingCost iid)
      pure i
    _ -> MarionTavares <$> liftRunMessage msg attrs
