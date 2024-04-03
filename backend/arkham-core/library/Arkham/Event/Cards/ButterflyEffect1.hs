module Arkham.Event.Cards.ButterflyEffect1 (butterflyEffect1, ButterflyEffect1 (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Prelude
import Data.Map.Strict qualified as Map

newtype ButterflyEffect1 = ButterflyEffect1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflyEffect1 :: EventCard ButterflyEffect1
butterflyEffect1 = event ButterflyEffect1 Cards.butterflyEffect1

instance RunMessage ButterflyEffect1 where
  runMessage msg e@(ButterflyEffect1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      performer <- fromJustNote "must be in skill test" <$> getSkillTestInvestigator
      yourCards <- getCommittableCards iid
      yourCommittedCards <- getCommittedCards iid
      you <- getPlayer iid
      performerPlayer <- getPlayer performer

      performerCards <- if performer /= iid then getCommittableCards performer else pure []
      performerCommittedCards <- if performer /= iid then getCommittedCards iid else pure []

      mustBeCommited <-
        filterM (`hasModifier` MustBeCommitted) (yourCommittedCards <> performerCommittedCards)

      push
        $ AskMap
        $ Map.fromList
        $ [
            ( you
            , ChooseOne
                $ map
                  (\card -> targetLabel (toCardId card) [CommitCard iid card])
                  yourCards
                <> [ targetLabel (toCardId card) [ReturnToHand iid (CardIdTarget $ toCardId card)]
                   | card <- yourCommittedCards
                   , card `notElem` mustBeCommited
                   ]
            )
          ]
        <> [ ( performerPlayer
             , ChooseOne
                $ map
                  (\card -> targetLabel (toCardId card) [CommitCard iid card])
                  performerCards
                <> [ targetLabel (toCardId card) [ReturnToHand iid (CardIdTarget $ toCardId card)]
                   | card <- performerCommittedCards
                   , card `notElem` mustBeCommited
                   ]
             )
           | performer /= iid
           ]

      pure e
    _ -> ButterflyEffect1 <$> runMessage msg attrs
