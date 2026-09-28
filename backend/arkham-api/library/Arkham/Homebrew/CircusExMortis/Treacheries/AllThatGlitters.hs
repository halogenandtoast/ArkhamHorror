module Arkham.Homebrew.CircusExMortis.Treacheries.AllThatGlitters (allThatGlitters) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (..), hasVice)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype AllThatGlitters = AllThatGlitters TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allThatGlitters :: TreacheryCard AllThatGlitters
allThatGlitters = treachery AllThatGlitters Cards.allThatGlitters

instance RunMessage AllThatGlitters where
  runMessage msg t@(AllThatGlitters attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      vice <- hasVice iid Opulence
      chooseNM iid (if vice then 2 else 1) $ withI18n do
        countVar 1 $ labeled "loseActions" $ loseActions iid attrs 1
        countVar 3 $ labeled "loseResources" $ loseResources iid attrs 3
        chooseTest #agility 3 $ revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      loseResources iid attrs 3
      pure t
    _ -> AllThatGlitters <$> liftRunMessage msg attrs
