module Arkham.Location.Cards.DunesOfTheSahara (dunesOfTheSahara) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DunesOfTheSahara = DunesOfTheSahara LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunesOfTheSahara :: LocationCard DunesOfTheSahara
dunesOfTheSahara = location DunesOfTheSahara Cards.dunesOfTheSahara 3 (PerPlayer 1)

instance HasAbilities DunesOfTheSahara where
  getAbilities (DunesOfTheSahara a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage DunesOfTheSahara where
  runMessage msg l@(DunesOfTheSahara attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withI18n $ chooseOneM iid do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled' "loseActions" $ loseActions iid (attrs.ability 1) 1
      pure l
    _ -> DunesOfTheSahara <$> liftRunMessage msg attrs
