module Arkham.Homebrew.CircusExMortis.Locations.SavageAltar (savageAltar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken, scenarioI18n, sealMoonTokenOn)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SavageAltar = SavageAltar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savageAltar :: LocationCard SavageAltar
savageAltar = location SavageAltar Cards.savageAltar 2 (PerPlayer 3)

instance HasAbilities SavageAltar where
  getAbilities (SavageAltar a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnBegins #when You

instance RunMessage SavageAltar where
  runMessage msg l@(SavageAltar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moonInBag <- selectAny moonToken
      chooseOneM iid $ scenarioI18n "bacchanalia" $ scope "savageAltar" do
        when moonInBag $ labeled "sealMoonToken" $ sealMoonTokenOn iid
        labeled "loseAction" $ loseActions iid (attrs.ability 1) 1
      pure l
    _ -> SavageAltar <$> liftRunMessage msg attrs
