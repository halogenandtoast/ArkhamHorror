module Arkham.Homebrew.CircusExMortis.Locations.ForgottenTrail (forgottenTrail) where

import Arkham.Helpers.Location (isAt)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestInvestigator, skillTestMatches)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ForgottenTrail = ForgottenTrail LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

forgottenTrail :: LocationCard ForgottenTrail
forgottenTrail = location ForgottenTrail Cards.forgottenTrail 3 (PerPlayer 1)

-- "Investigators at Forgotten Trail get +1 skill value during tests on encounter
-- cards." Only the investigator performing the test gets it, so this rides the
-- skill test investigator rather than everyone here.
instance HasModifiersFor ForgottenTrail where
  getModifiersFor (ForgottenTrail a) =
    getSkillTestInvestigator >>= traverse_ \iid ->
      maybeModified_ a iid do
        guard a.revealed
        guardM $ iid `isAt` a
        st <- MaybeT getSkillTest
        liftGuardM $ skillTestMatches iid (toSource a) st SkillTestOnEncounterCard
        pure [AnySkillValue 1]

instance RunMessage ForgottenTrail where
  runMessage msg (ForgottenTrail attrs) = ForgottenTrail <$> runMessage msg attrs
