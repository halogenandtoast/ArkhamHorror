module Arkham.Location.Cards.RuinsOfCarcosaInhabitantOfCarcosa (
  ruinsOfCarcosaInhabitantOfCarcosa,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype RuinsOfCarcosaInhabitantOfCarcosa = RuinsOfCarcosaInhabitantOfCarcosa LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaInhabitantOfCarcosa
  :: LocationCard RuinsOfCarcosaInhabitantOfCarcosa
ruinsOfCarcosaInhabitantOfCarcosa =
  locationWith
    RuinsOfCarcosaInhabitantOfCarcosa
    Cards.ruinsOfCarcosaInhabitantOfCarcosa
    2
    (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasAbilities RuinsOfCarcosaInhabitantOfCarcosa where
  getAbilities (RuinsOfCarcosaInhabitantOfCarcosa a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ DiscoveringLastClue #after You (be a)

instance RunMessage RuinsOfCarcosaInhabitantOfCarcosa where
  runMessage msg l@(RuinsOfCarcosaInhabitantOfCarcosa attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.inhabitantOfCarcosa
      pure . RuinsOfCarcosaInhabitantOfCarcosa $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfCarcosaInhabitantOfCarcosa <$> liftRunMessage msg attrs
