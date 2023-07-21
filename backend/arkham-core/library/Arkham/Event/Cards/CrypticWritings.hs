module Arkham.Event.Cards.CrypticWritings (
  crypticWritings,
  CrypticWritings (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype CrypticWritings = CrypticWritings EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticWritings :: EventCard CrypticWritings
crypticWritings = event CrypticWritings Cards.crypticWritings

instance HasAbilities CrypticWritings where
  getAbilities (CrypticWritings x) =
    [ restrictedAbility x 1 (InYourHand <> DuringTurn You) $
        ReactionAbility
          ( DrawCard
              Timing.After
              You
              (BasicCardMatch $ CardWithId $ toCardId x)
              AnyDeck
          )
          Free
    ]

instance RunMessage CrypticWritings where
  runMessage msg e@(CrypticWritings attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ TakeResources iid 2 (toSource attrs) False
      pure e
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 windows' _)
      | iid' == iid -> do
          push $ InitiatePlayCard iid (toCard attrs) Nothing windows' False
          pure e
    _ -> CrypticWritings <$> runMessage msg attrs
