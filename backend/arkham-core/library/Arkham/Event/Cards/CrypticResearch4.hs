module Arkham.Event.Cards.CrypticResearch4 where

import Arkham.Prelude

import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance RunMessage CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      iids <-
        select
          =<< guardAffectsOthers iid (colocatedWith iid <> can.draw.cards)
      let investigators = map (\iid' -> (iid', drawCards iid' attrs 3)) iids
      player <- getPlayer iid
      push $ chooseOne player $ [targetLabel iid' [drawing] | (iid', drawing) <- investigators]
      pure e
    _ -> CrypticResearch4 <$> runMessage msg attrs
