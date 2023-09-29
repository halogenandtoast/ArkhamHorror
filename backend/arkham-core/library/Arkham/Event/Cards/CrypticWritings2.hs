module Arkham.Event.Cards.CrypticWritings2 (
  crypticWritings2,
  CrypticWritings2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype CrypticWritings2 = CrypticWritings2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticWritings2 :: EventCard CrypticWritings2
crypticWritings2 = event CrypticWritings2 Cards.crypticWritings2

instance HasAbilities CrypticWritings2 where
  getAbilities (CrypticWritings2 x) =
    [ restrictedAbility x 1 (InYourHand <> DuringTurn You)
        $ ReactionAbility
          ( DrawCard
              Timing.After
              You
              (BasicCardMatch $ CardWithId $ toCardId x)
              AnyDeck
          )
          Free
    ]

instance RunMessage CrypticWritings2 where
  runMessage msg e@(CrypticWritings2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      hasTenOrMoreCards <- fieldMap InvestigatorHand ((>= 10) . length) iid
      let n = if hasTenOrMoreCards then 4 else 3
      push $ TakeResources iid n (toSource attrs) False
      pure e
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 windows' _)
      | iid' == iid -> do
          push $ InitiatePlayCard iid (toCard attrs) Nothing windows' False
          pure e
    _ -> CrypticWritings2 <$> runMessage msg attrs
