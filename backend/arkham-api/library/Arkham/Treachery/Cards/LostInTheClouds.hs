module Arkham.Treachery.Cards.LostInTheClouds (lostInTheClouds) where

import Arkham.I18n (countVar, unscoped)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ObsidianCanyons.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostInTheClouds = LostInTheClouds TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostInTheClouds :: TreacheryCard LostInTheClouds
lostInTheClouds = treachery LostInTheClouds Cards.lostInTheClouds

instance RunMessage LostInTheClouds where
  runMessage msg t@(LostInTheClouds attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ scenarioI18n do
        unscoped
          $ countVar 1
          $ labeled' "placeAgendaDoomCanAdvance"
          $ placeDoomOnAgendaAndCheckAdvance 1
        labeled' "lostInTheClouds.eachInvestigatorTakesHorror" do
          eachInvestigator \iid' -> directHorror iid' attrs 2
        -- "Resolve the Forced effect on the Eastern Winds/Western Winds story
        -- card as if you had drawn a non-[elder sign] symbol token" — i.e. skip
        -- straight to the gust, bypassing the reveal.
        labeled' "lostInTheClouds.resolveWinds" blowWindsFromStory
      pure t
    _ -> LostInTheClouds <$> liftRunMessage msg attrs
