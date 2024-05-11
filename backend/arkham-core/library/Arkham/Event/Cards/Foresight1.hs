module Arkham.Event.Cards.Foresight1 (foresight1, Foresight1 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Query (getPlayer)
import Arkham.Message (chooseOneDropDown)
import Arkham.Name

newtype Foresight1 = Foresight1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foresight1 :: EventCard Foresight1
foresight1 = event Foresight1 Cards.foresight1

instance RunMessage Foresight1 where
  runMessage msg e@(Foresight1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      cards <- findAllCards (const True)
      let cardNames = nub $ sort $ map toTitle cards
      player <- getPlayer iid
      push $ chooseOneDropDown player [(name, Run []) | name <- cardNames]
      pure e
    _ -> Foresight1 <$> lift (runMessage msg attrs)
