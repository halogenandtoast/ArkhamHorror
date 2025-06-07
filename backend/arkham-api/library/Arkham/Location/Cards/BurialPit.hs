module Arkham.Location.Cards.BurialPit (burialPit) where

import Arkham.Ability
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDoomOfEztli.Helpers

newtype BurialPit = BurialPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burialPit :: LocationCard BurialPit
burialPit =
  location BurialPit Cards.burialPit 3 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities BurialPit where
  getAbilities (BurialPit a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Explored #after You Anywhere (SuccessfulExplore $ be a)

instance RunMessage BurialPit where
  runMessage msg l@(BurialPit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      repeated 2 do
        chooseOneM iid $ scenarioI18n do
          labeled "burialPit.draw" $ drawEncounterCard iid attrs
          labeled "burialPit.doom" $ placeDoom (attrs.ability 1) attrs 1
      pure l
    _ -> BurialPit <$> liftRunMessage msg attrs
