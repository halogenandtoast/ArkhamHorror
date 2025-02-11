module Arkham.Investigator.Cards.MarionTavares (marionTavares) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (PlayCard)
import Arkham.Matcher hiding (DuringTurn)
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
        $ restricted a 1 (Self <> DuringTurn You <> youExist can.draw.cards)
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
    ResetGame -> do
      result <- liftRunMessage msg attrs
      pure $ MarionTavares $ setSlots result
    _ -> MarionTavares <$> liftRunMessage msg attrs
