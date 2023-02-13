module Arkham.Event.Cards.ScroungeForSupplies
  ( scroungeForSupplies
  , ScroungeForSupplies(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype ScroungeForSupplies = ScroungeForSupplies EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scroungeForSupplies :: EventCard ScroungeForSupplies
scroungeForSupplies = event ScroungeForSupplies Cards.scroungeForSupplies

instance RunMessage ScroungeForSupplies where
  runMessage msg e@(ScroungeForSupplies attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectList $ InDiscardOf (InvestigatorWithId iid) <> BasicCardMatch
          (CardWithLevel 0)
      when
        (null targets)
        (error "ScroungeForSupplies expected level 0 card in discard")
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel (CardIdTarget $ toCardId target) [AddToHand iid target]
          | target <- targets
          ]
        ]
    _ -> ScroungeForSupplies <$> runMessage msg attrs
