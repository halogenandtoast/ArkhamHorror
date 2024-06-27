module Arkham.Event.Cards.CrypticWritings2 (crypticWritings2, CrypticWritings2 (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype CrypticWritings2 = CrypticWritings2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticWritings2 :: EventCard CrypticWritings2
crypticWritings2 = event CrypticWritings2 Cards.crypticWritings2

instance HasAbilities CrypticWritings2 where
  getAbilities (CrypticWritings2 x) =
    [ restrictedAbility x 1 (InYourHand <> DuringTurn (can.gain.resources You))
        $ freeReaction
        $ DrawCard #after You (basic $ CardWithId x.cardId) AnyDeck
    ]

instance RunMessage CrypticWritings2 where
  runMessage msg e@(CrypticWritings2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      hasTenOrMoreCards <- fieldMap InvestigatorHand ((>= 10) . length) iid
      gainResourcesIfCan iid attrs $ if hasTenOrMoreCards then 4 else 3
      pure e
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ InitiatePlayCard iid (toCard attrs) Nothing NoPayment (defaultWindows iid) False
      pure e
    _ -> CrypticWritings2 <$> lift (runMessage msg attrs)
