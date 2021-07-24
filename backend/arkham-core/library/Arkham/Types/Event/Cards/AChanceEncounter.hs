module Arkham.Types.Event.Cards.AChanceEncounter
  ( aChanceEncounter
  , AChanceEncounter(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance HasActions env AChanceEncounter where
  getActions iid window (AChanceEncounter attrs) = getActions iid window attrs

instance HasModifiersFor env AChanceEncounter

instance (HasSet InvestigatorId env (), HasList DiscardedPlayerCard env InvestigatorId) => RunMessage env AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == toId attrs -> do
      investigatorIds <- getInvestigatorIds
      discards <-
        map PlayerCard
        . concat
        <$> traverse
              (fmap (map unDiscardedPlayerCard) . getList)
              investigatorIds
      let filteredDiscards = filter (elem Ally . toTraits) discards
      e <$ pushAll
        [ FocusCards filteredDiscards
        , chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId card)
              [ PutCardIntoPlay iid card Nothing
              , RemoveFromDiscard iid (toCardId card)
              , CreateEffect
                "02270"
                Nothing
                (toSource attrs)
                (CardIdTarget $ toCardId card)
              ]
          | card <- filteredDiscards
          ]
        , UnfocusCards
        , Discard (toTarget attrs)
        ]
    _ -> AChanceEncounter <$> runMessage msg attrs
