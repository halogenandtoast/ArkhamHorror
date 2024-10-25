module Arkham.Event.Events.FoolMeOnce1 (foolMeOnce1, FoolMeOnce1 (..)) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Id
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Types (Field (..))
import Arkham.Window hiding (DrawCard, PlaceUnderneath)

newtype FoolMeOnce1 = FoolMeOnce1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foolMeOnce1 :: EventCard FoolMeOnce1
foolMeOnce1 = event FoolMeOnce1 Cards.foolMeOnce1

getTreachery :: [Window] -> (BatchId, TreacheryId)
getTreachery (w@(windowType -> WouldBeDiscarded (TreacheryTarget treacheryId)) : _) = (fromJustNote "missing batchId" $ windowBatchId w, treacheryId)
getTreachery (_ : rest) = getTreachery rest
getTreachery [] = error "impossible"

instance HasAbilities FoolMeOnce1 where
  getAbilities (FoolMeOnce1 attrs) =
    [ restricted attrs 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when Anyone (basic $ mapOneOf cardIs $ eventCardsUnderneath attrs) AnyDeck)
          (discardCost attrs)
    ]

instance RunMessage FoolMeOnce1 where
  runMessage msg e@(FoolMeOnce1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      let (batchId, treachery) = getTreachery attrs.windows
      card <- field TreacheryCard treachery
      pushAll
        [ RemoveTreachery treachery
        , IgnoreBatch batchId
        , PlaceEvent eid (InPlayArea iid)
        , PlaceUnderneath (toTarget attrs) [card]
        ]
      pure e
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> treachery) _ -> do
      cancelRevelation (attrs.ability 1) treachery
      pure e
    _ -> FoolMeOnce1 <$> liftRunMessage msg attrs
