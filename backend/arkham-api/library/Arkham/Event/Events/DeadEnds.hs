module Arkham.Event.Events.DeadEnds (deadEnds) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Builder
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (getBatchId)

newtype DeadEnds = DeadEnds EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadEnds :: EventCard DeadEnds
deadEnds = event DeadEnds Cards.deadEnds

instance HasAbilities DeadEnds where
  getAbilities (DeadEnds a) =
    [ mkAbility a 1 $ forced (AmongSearchedCards You)
    , restricted a 2 InYourHand $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    ]

instance RunMessage DeadEnds where
  runMessage msg e@(DeadEnds attrs) = runQueueT $ case msg of
    InSearch (UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _) -> do
      let card = fromJustNote "is player card" $ preview _PlayerCard (toCard attrs)
      push $ RemoveCardFromSearch iid (toCardId card)
      drawCard iid card
      cancelBatch batchId
      push $ CancelSearch (toTarget iid) -- shuffles the deck
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 2) | iid' == iid -> do
      withSource (attrs.ability 2) $ effect iid do
        during #resolution
        apply $ XPModifier "Dead Ends" (-2)
      pure e
    _ -> DeadEnds <$> liftRunMessage msg attrs
