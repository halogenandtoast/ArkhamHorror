module Arkham.Event.Cards.CrypticWritings (crypticWritings, CrypticWritings (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude
import Arkham.Window (defaultWindows)

newtype CrypticWritings = CrypticWritings EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticWritings :: EventCard CrypticWritings
crypticWritings = event CrypticWritings Cards.crypticWritings

instance HasAbilities CrypticWritings where
  getAbilities (CrypticWritings x) =
    [ restrictedAbility x 1 (InYourHand <> DuringTurn You)
        $ freeReaction
        $ DrawCard #after You (basic $ CardWithId $ toCardId x) AnyDeck
    ]

instance RunMessage CrypticWritings where
  runMessage msg e@(CrypticWritings attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ TakeResources iid 2 (toSource attrs) False
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ InitiatePlayCard iid (toCard attrs) Nothing NoPayment (defaultWindows iid) False
      pure e
    _ -> CrypticWritings <$> runMessage msg attrs
