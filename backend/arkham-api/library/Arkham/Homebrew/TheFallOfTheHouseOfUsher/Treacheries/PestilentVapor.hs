module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Treacheries.PestilentVapor (pestilentVapor) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards, randomDiscard)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers (scenarioI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype PestilentVapor = PestilentVapor TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pestilentVapor :: TreacheryCard PestilentVapor
pestilentVapor = treachery PestilentVapor Cards.pestilentVapor

instance RunMessage PestilentVapor where
  runMessage msg t@(PestilentVapor attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ scenarioI18n $ scope "pestilentVapor" do
        labeled "randomDiscard" $ selectEach (colocatedWith iid) (`randomDiscard` attrs)
        labeled "chooseAndDiscard" $ selectEach (colocatedWith iid) \iid' -> chooseAndDiscardCards iid' attrs 2
        labeled "discardTopOfDeck" $ eachInvestigator \iid' -> discardTopOfDeck iid' attrs 3
      pure t
    _ -> PestilentVapor <$> liftRunMessage msg attrs
