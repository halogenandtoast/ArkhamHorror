module Arkham.Location.Cards.RecessesOfYourOwnMind (recessesOfYourOwnMind) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story

newtype RecessesOfYourOwnMind = RecessesOfYourOwnMind LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

recessesOfYourOwnMind :: LocationCard RecessesOfYourOwnMind
recessesOfYourOwnMind =
  locationWith RecessesOfYourOwnMind Cards.recessesOfYourOwnMind 3 (PerPlayer 2)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

instance HasModifiersFor RecessesOfYourOwnMind where
  getModifiersFor (RecessesOfYourOwnMind a) = do
    modifySelect a (InvestigatorAt $ be a) [BaseSkillOf #willpower 0]
    modifySelf a [CountsAsDifferentLocation]

instance RunMessage RecessesOfYourOwnMind where
  runMessage msg (RecessesOfYourOwnMind attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theWriter
      pure . RecessesOfYourOwnMind $ attrs & canBeFlippedL .~ False
    _ -> RecessesOfYourOwnMind <$> liftRunMessage msg attrs
