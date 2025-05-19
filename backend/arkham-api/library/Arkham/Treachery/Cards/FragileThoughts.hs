module Arkham.Treachery.Cards.FragileThoughts (fragileThoughts) where

import Arkham.Card
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Investigator.Projection ()
import Arkham.Location.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FragileThoughts = FragileThoughts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragileThoughts :: TreacheryCard FragileThoughts
fragileThoughts = treachery FragileThoughts Cards.fragileThoughts

handMatcher :: CardMatcher
handMatcher = #event <> NonWeakness

instance RunMessage FragileThoughts where
  runMessage msg t@(FragileThoughts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        shroud <- fromMaybe 0 <$> lid.shroud
        ok <- any (`cardMatch` handMatcher) <$> iid.hand
        if shroud > 0 && ok
          then doStep shroud msg
          else gainSurge attrs
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      events <- iid.hand.filter handMatcher
      chooseOneM iid do
        targets events \card -> do
          toDiscardBy iid attrs card.id
          doStep (n - card.printedCost) msg'
      pure t
    _ -> FragileThoughts <$> liftRunMessage msg attrs
