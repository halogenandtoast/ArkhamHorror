module Arkham.Location.Cards.RuinsOfCarcosaTheCoffin (ruinsOfCarcosaTheCoffin) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype RuinsOfCarcosaTheCoffin = RuinsOfCarcosaTheCoffin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaTheCoffin :: LocationCard RuinsOfCarcosaTheCoffin
ruinsOfCarcosaTheCoffin =
  locationWith RuinsOfCarcosaTheCoffin Cards.ruinsOfCarcosaTheCoffin 2 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasAbilities RuinsOfCarcosaTheCoffin where
  getAbilities (RuinsOfCarcosaTheCoffin a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage RuinsOfCarcosaTheCoffin where
  runMessage msg l@(RuinsOfCarcosaTheCoffin attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theCoffin
      pure . RuinsOfCarcosaTheCoffin $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfCarcosaTheCoffin <$> liftRunMessage msg attrs
