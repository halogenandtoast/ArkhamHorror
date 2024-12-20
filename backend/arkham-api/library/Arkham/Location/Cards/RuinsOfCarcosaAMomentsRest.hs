module Arkham.Location.Cards.RuinsOfCarcosaAMomentsRest (ruinsOfCarcosaAMomentsRest) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype RuinsOfCarcosaAMomentsRest = RuinsOfCarcosaAMomentsRest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaAMomentsRest :: LocationCard RuinsOfCarcosaAMomentsRest
ruinsOfCarcosaAMomentsRest =
  locationWith RuinsOfCarcosaAMomentsRest Cards.ruinsOfCarcosaAMomentsRest 2 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasAbilities RuinsOfCarcosaAMomentsRest where
  getAbilities (RuinsOfCarcosaAMomentsRest a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage RuinsOfCarcosaAMomentsRest where
  runMessage msg l@(RuinsOfCarcosaAMomentsRest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.aMomentsRest
      pure . RuinsOfCarcosaAMomentsRest $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfCarcosaAMomentsRest <$> liftRunMessage msg attrs
