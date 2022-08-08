module Arkham.Event.Cards.AChanceEncounter
  ( aChanceEncounter
  , AChanceEncounter(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Trait

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance RunMessage AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      investigatorIds <-
        filterM
            (fmap (notElem CardsCannotLeaveYourDiscardPile)
            . getModifiers
            . InvestigatorTarget
            )
          =<< getInvestigatorIds
      discards <-
        concat
          <$> traverse
                (fieldMap InvestigatorDiscard (map PlayerCard))
                investigatorIds
      let filteredDiscards = filter (elem Ally . toTraits) discards
      e <$ pushAll
        [ FocusCards filteredDiscards
        , chooseOne
          iid
          [ TargetLabel
              (CardTarget card)
              [ PutCardIntoPlay iid card Nothing windows'
              , RemoveFromDiscard iid (toCardId card)
              , CreateEffect
                "02270"
                Nothing
                (toSource attrs)
                (CardTarget card)
              ]
          | card <- filteredDiscards
          ]
        , UnfocusCards
        , Discard (toTarget attrs)
        ]
    _ -> AChanceEncounter <$> runMessage msg attrs
