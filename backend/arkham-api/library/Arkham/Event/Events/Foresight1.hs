module Arkham.Event.Events.Foresight1 (foresight1) where

import Arkham.Classes.HasGame
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted hiding (cardDrawModifier)
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Modifiers
import Arkham.Name
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

-- Needs to affect search/draw
-- If the drawn card is the named card, that investigator may either (choose one):
-- - Cancel that card's effects and discard it.
-- - Immediately play that card at -2 cost.

newtype Foresight1 = Foresight1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foresight1 :: EventCard Foresight1
foresight1 = event Foresight1 Cards.foresight1

allCardNames :: HasGame m => m [Text]
allCardNames = nub . sort . map toTitle <$> findAllCards (const True)

getWindowInvestigator :: [Window] -> (InvestigatorId, CardDrawId)
getWindowInvestigator [] = error "getWindowInvestigator: empty list"
getWindowInvestigator ((windowType -> Window.WouldDrawCard iid cid _) : _) = (iid, cid)
getWindowInvestigator (_ : ws) = getWindowInvestigator ws

-- The actual cancel or play part is encoded in the card draw kind `Foresight`

instance RunMessage Foresight1 where
  runMessage msg e@(Foresight1 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ (getWindowInvestigator -> (iid', cid)) _ | eid == toId attrs -> do
      cardNames <- allCardNames

      chooseOneDropDown iid =<< for cardNames \name -> do
        enabled <- cardDrawModifier cid attrs iid' (Foresight name)
        pure (name, enabled)
      pure e
    _ -> Foresight1 <$> liftRunMessage msg attrs
