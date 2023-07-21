module Arkham.Event.Cards.TrueSurvivor3 (
  trueSurvivor3,
  TrueSurvivor3 (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype TrueSurvivor3 = TrueSurvivor3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trueSurvivor3 :: EventCard TrueSurvivor3
trueSurvivor3 = event TrueSurvivor3 Cards.trueSurvivor3

instance RunMessage TrueSurvivor3 where
  runMessage msg e@(TrueSurvivor3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      targets <-
        selectList $
          InDiscardOf (InvestigatorWithId iid)
            <> BasicCardMatch
              (CardWithTrait Innate)
      when
        (null targets)
        (error "ScroungeForSupplies expected level 0 card in discard")
      e
        <$ pushAll
          [ chooseN
              iid
              3
              [ TargetLabel (CardIdTarget $ toCardId target) [addToHand iid target]
              | target <- targets
              ]
          ]
    _ -> TrueSurvivor3 <$> runMessage msg attrs
