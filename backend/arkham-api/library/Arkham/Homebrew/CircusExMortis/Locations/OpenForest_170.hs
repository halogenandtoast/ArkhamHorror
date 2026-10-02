module Arkham.Homebrew.CircusExMortis.Locations.OpenForest_170 (
  openForest_170,
  cannotTriggerFreeAbilitiesWhile,
) where

import Arkham.Action (Action)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestInvestigator, isSkillTestAt)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype OpenForest_170 = OpenForest_170 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

openForest_170 :: LocationCard OpenForest_170
openForest_170 = location OpenForest_170 Cards.openForest_170 2 (PerPlayer 1)

-- | Shared by all three Open Forest copies.
cannotTriggerFreeAbilitiesWhile :: HasModifiersM m => Action -> LocationAttrs -> m ()
cannotTriggerFreeAbilitiesWhile action a = do
  active <- andM [(== Just action) <$> getSkillTestAction, isSkillTestAt a]
  when active
    $ traverse_ (\iid -> modified_ a iid [CannotTriggerFastAbilities])
    =<< getSkillTestInvestigator

instance HasModifiersFor OpenForest_170 where
  getModifiersFor (OpenForest_170 a) = cannotTriggerFreeAbilitiesWhile #investigate a

instance RunMessage OpenForest_170 where
  runMessage msg (OpenForest_170 attrs) = runQueueT $ OpenForest_170 <$> liftRunMessage msg attrs
