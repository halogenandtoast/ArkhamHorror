module Arkham.Event.Cards.CrypticResearch4 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype CrypticResearch4 = CrypticResearch4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crypticResearch4 :: EventCard CrypticResearch4
crypticResearch4 = event CrypticResearch4 Cards.crypticResearch4

instance RunMessage CrypticResearch4 where
  runMessage msg e@(CrypticResearch4 attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      iids <- selectList $ colocatedWith iid
      investigators <- forToSnd iids $ \iid' -> drawCards iid' attrs 3
      player <- getPlayer iid
      push $ chooseOne player $ [targetLabel iid' [drawing] | (iid', drawing) <- investigators]
      pure e
    _ -> CrypticResearch4 <$> runMessage msg attrs
