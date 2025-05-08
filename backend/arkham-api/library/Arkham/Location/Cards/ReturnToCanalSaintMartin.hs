module Arkham.Location.Cards.ReturnToCanalSaintMartin (returnToCanalSaintMartin) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.SkillTest (getSkillTestMatchingSkillIcons)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ReturnToCanalSaintMartin = ReturnToCanalSaintMartin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor ReturnToCanalSaintMartin where
  getModifiersFor (ReturnToCanalSaintMartin a) = do
    kinds <- getSkillTestMatchingSkillIcons
    if
      | #agility `member` kinds ->
          modifySelect a (InvestigatorAt $ be a) [UseSkillInsteadOf #agility #combat]
      | #combat `member` kinds ->
          modifySelect a (InvestigatorAt $ be a) [UseSkillInsteadOf #combat #agility]
      | otherwise -> pure ()

returnToCanalSaintMartin :: LocationCard ReturnToCanalSaintMartin
returnToCanalSaintMartin = location ReturnToCanalSaintMartin Cards.returnToCanalSaintMartin 3 (PerPlayer 1)

instance RunMessage ReturnToCanalSaintMartin where
  runMessage msg (ReturnToCanalSaintMartin attrs) = ReturnToCanalSaintMartin <$> runMessage msg attrs
