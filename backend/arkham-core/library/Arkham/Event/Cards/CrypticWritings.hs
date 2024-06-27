module Arkham.Event.Cards.CrypticWritings (crypticWritings, CrypticWritings (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Window (defaultWindows)

newtype CrypticWritings = CrypticWritings EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticWritings :: EventCard CrypticWritings
crypticWritings = event CrypticWritings Cards.crypticWritings

instance HasAbilities CrypticWritings where
  getAbilities (CrypticWritings x) =
    [ restrictedAbility x 1 (InYourHand <> DuringTurn (can.gain.resources You))
        $ freeReaction
        $ DrawCard #after You (basic $ CardWithId x.cardId) AnyDeck
    ]

instance RunMessage CrypticWritings where
  runMessage msg e@(CrypticWritings attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResourcesIfCan iid attrs 2
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ InitiatePlayCard iid (toCard attrs) Nothing NoPayment (defaultWindows iid) False
      pure e
    _ -> CrypticWritings <$> lift (runMessage msg attrs)
