module Arkham.Homebrew.CircusExMortis.Treacheries.DrinkAndBeMerry (drinkAndBeMerry) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (..), hasVice)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype DrinkAndBeMerry = DrinkAndBeMerry TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drinkAndBeMerry :: TreacheryCard DrinkAndBeMerry
drinkAndBeMerry = treachery DrinkAndBeMerry Cards.drinkAndBeMerry

instance RunMessage DrinkAndBeMerry where
  runMessage msg t@(DrinkAndBeMerry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      vice <- hasVice iid Revelry
      chooseNM iid (if vice then 2 else 1) $ withI18n do
        countVar 1 $ labeled "loseActions" $ loseActions iid attrs 1
        countVar 2 $ labeled "takeHorror" $ assignHorror iid attrs 2
        chooseTest #willpower 3 $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> DrinkAndBeMerry <$> liftRunMessage msg attrs
