module Arkham.Act.Cards.DawnOfTheFinalDay (dawnOfTheFinalDay) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype DawnOfTheFinalDay = DawnOfTheFinalDay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dawnOfTheFinalDay :: ActCard DawnOfTheFinalDay
dawnOfTheFinalDay = act (1, A) DawnOfTheFinalDay Cards.dawnOfTheFinalDay Nothing

instance RunMessage DawnOfTheFinalDay where
  runMessage msg a@(DawnOfTheFinalDay attrs) = runQueueT $ case msg of
    KonamiCode pid -> campaignI18n do
      f <- getLogger
      selectEach (InvestigatorIsPlayer pid) \iid ->
        semaphore iid do
          gameModifier attrs iid Semaphore
          liftIO $ f (ClientCardOnly pid "The Bliss" (toJSON $ flipCard $ toCard attrs))
          chooseOneM iid do
            unscoped (countVar 1 $ labeled' "sufferMentalTrauma") do
              sufferMentalTrauma iid 1
              gainXp iid attrs (ikey "xp.bonus") 2
            labeled' "dawnOfTheFinalDay.searchForRandomBasicMadnessWeakness" do
              searchCollectionForRandomBasicWeakness iid attrs [Madness]
              gainXp iid attrs (ikey "xp.bonus") 2
      pure a
    RequestedPlayerCard iid (isSource attrs -> True) (Just card) _ -> do
      focusCards [card] do
        continue_ iid
        addCampaignCardToDeck iid ShuffleIn card
      pure a
    _ -> DawnOfTheFinalDay <$> liftRunMessage msg attrs
