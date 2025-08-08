module Arkham.Event.Events.YouOweMeOne (youOweMeOne) where

import Arkham.Card
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Playable (getIsPlayable)
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype YouOweMeOne = YouOweMeOne EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youOweMeOne :: EventCard YouOweMeOne
youOweMeOne = event YouOweMeOne Cards.youOweMeOne

instance RunMessage YouOweMeOne where
  runMessage msg e@(YouOweMeOne attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      others <- select $ NotInvestigator (InvestigatorWithId iid)
      chooseOrRunOneM iid $ targets others $ handleTarget iid attrs
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') -> do
      cards <- field InvestigatorHand iid'
      relevantCards <-
        filterM (getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid))
          $ filter (`cardMatch` (NonWeakness <> NonSignature)) cards
      focusCards cards do
        chooseOneM iid do
          labeled "Do not play a card" nothing
          targets relevantCards \card -> do
            playCardPayingCost iid card
            drawCards iid attrs 1
            drawCards iid' attrs 1
      pure e
    _ -> YouOweMeOne <$> liftRunMessage msg attrs
