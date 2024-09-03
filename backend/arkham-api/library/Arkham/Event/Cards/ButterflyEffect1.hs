module Arkham.Event.Cards.ButterflyEffect1 (butterflyEffect1, ButterflyEffect1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getPlayer)
import Arkham.Helpers.SkillTest (getCommittableCards, getCommittedCards, getSkillTestInvestigator)
import Data.Map.Strict qualified as Map

newtype ButterflyEffect1 = ButterflyEffect1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflyEffect1 :: EventCard ButterflyEffect1
butterflyEffect1 = event ButterflyEffect1 Cards.butterflyEffect1

instance RunMessage ButterflyEffect1 where
  runMessage msg e@(ButterflyEffect1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
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
                $ map (\card -> targetLabel card [CommitCard iid card]) yourCards
                <> [ targetLabel card [ReturnToHand iid (CardIdTarget card.id)]
                   | card <- yourCommittedCards
                   , card `notElem` mustBeCommited
                   ]
            )
          ]
        <> [ ( performerPlayer
             , ChooseOne
                $ map (\card -> targetLabel card [CommitCard iid card]) performerCards
                <> [ targetLabel card [ReturnToHand iid (CardIdTarget card.id)]
                   | card <- performerCommittedCards
                   , card `notElem` mustBeCommited
                   ]
             )
           | performer /= iid
           ]

      pure e
    _ -> ButterflyEffect1 <$> liftRunMessage msg attrs
