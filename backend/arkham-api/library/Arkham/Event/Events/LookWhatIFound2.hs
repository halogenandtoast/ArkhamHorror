module Arkham.Event.Events.LookWhatIFound2 where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype LookWhatIFound2 = LookWhatIFound2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound2 :: EventCard LookWhatIFound2
lookWhatIFound2 = event LookWhatIFound2 Cards.lookWhatIFound2

instance RunMessage LookWhatIFound2 where
  runMessage msg e@(LookWhatIFound2 attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      twice $ do_ msg
      pure e
    Do (PlayThisEvent iid (is attrs -> True)) -> do
      locations <- select $ orConnected iid <> LocationWithAnyClues
      chooseTargetM iid locations \lid -> discoverAt NotInvestigate iid attrs lid 1
      pure e
    _ -> LookWhatIFound2 <$> liftRunMessage msg attrs
