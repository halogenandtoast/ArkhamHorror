module Arkham.Event.Cards.DelveTooDeep (
  delveTooDeep,
  DelveTooDeep (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype DelveTooDeep = DelveTooDeep EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delveTooDeep :: EventCard DelveTooDeep
delveTooDeep = event DelveTooDeep Cards.delveTooDeep

instance RunMessage DelveTooDeep where
  runMessage msg e@(DelveTooDeep attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      investigators <-
        traverse (traverseToSnd getPlayer)
          =<< selectList
          =<< guardAffectsOthers iid UneliminatedInvestigator
      eachInvestigator <- getInvestigators
      pushAll
        $ [ chooseOne
            player
            [ TargetLabel
                EncounterDeckTarget
                [InvestigatorDrawEncounterCard iid']
            ]
          | (iid', player) <- investigators
          ]
        <> [SetActiveInvestigator iid]
        <> [AddToVictory (toTarget attrs) | map fst investigators == eachInvestigator]
      pure e
    _ -> DelveTooDeep <$> runMessage msg attrs
