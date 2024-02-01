module Arkham.Event.Cards.FoolMeOnce1 (
  foolMeOnce1,
  FoolMeOnce1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Types (Field (..))
import Arkham.Window hiding (DrawCard, PlaceUnderneath)

newtype FoolMeOnce1 = FoolMeOnce1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

foolMeOnce1 :: EventCard FoolMeOnce1
foolMeOnce1 = event FoolMeOnce1 Cards.foolMeOnce1

getTreachery :: [Window] -> (BatchId, TreacheryId)
getTreachery (w@(windowType -> WouldBeDiscarded (TreacheryTarget treacheryId)) : _) = (fromJustNote "missing batchId" $ windowBatchId w, treacheryId)
getTreachery (_ : rest) = getTreachery rest
getTreachery [] = error "impossible"

instance HasAbilities FoolMeOnce1 where
  getAbilities (FoolMeOnce1 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          ( DrawCard
              #when
              Anyone
              (basic $ oneOf $ map cardIs $ eventCardsUnderneath attrs)
              AnyDeck
          )
          (discardCost attrs)
    ]

instance RunMessage FoolMeOnce1 where
  runMessage msg e@(FoolMeOnce1 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ (getTreachery -> (batchId, treachery)) _ | eid == toId attrs -> do
      card <- field TreacheryCard treachery
      pushAll
        [ RemoveTreachery treachery
        , IgnoreBatch batchId
        , PlaceEvent iid eid (InPlayArea iid)
        , PlaceUnderneath (toTarget attrs) [card]
        ]
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ CancelNext (toSource attrs) RevelationMessage
      pure e
    _ -> FoolMeOnce1 <$> runMessage msg attrs
