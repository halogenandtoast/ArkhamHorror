module Arkham.Homebrew.AgesUnwound.Treacheries.WatchYouBreak (watchYouBreak) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers (timeRunsOutI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Madness, Pact))
import Arkham.Treachery.Import.Lifted

newtype WatchYouBreak = WatchYouBreak TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

watchYouBreak :: TreacheryCard WatchYouBreak
watchYouBreak = treachery WatchYouBreak Cards.watchYouBreak

instance RunMessage WatchYouBreak where
  runMessage msg t@(WatchYouBreak attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ timeRunsOutI18n $ scope "watchYouBreak" do
        labeled "placeDoom" $ placeDoomOnAgendaAndCheckAdvance 1
        labeled "drawAWeakness" $ searchCollectionForRandomBasicWeakness iid attrs [Pact, Madness]
      pure t
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      drawCard iid card
      pure t
    _ -> WatchYouBreak <$> liftRunMessage msg attrs
