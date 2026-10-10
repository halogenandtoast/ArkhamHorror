module Arkham.Homebrew.AgesUnwound.Locations.SportsField_172 (sportsField_172) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (act1c)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SportsField_172 = SportsField_172 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Scenario VI's starting location on every branch but /Featureless Streets/.
sportsField_172 :: LocationCard SportsField_172
sportsField_172 = symbolLabel $ location SportsField_172 Cards.sportsField_172 2 (PerPlayer 1)

{- | "Forced - At the end of the round, if act 1c is in play: Each investigator at
Sports Field must test [agility] (3). Each investigator who fails takes 2
horror."

"Each investigator at Sports Field" is the effect, not the trigger, so the
ability is not @Here@-restricted -- it fires once for the location and the
handler loops. The exists-criterion keeps it from firing with nobody standing
here.
-}
instance HasAbilities SportsField_172 where
  getAbilities (SportsField_172 a) =
    extendRevealed1 a
      $ restricted a 1 (ActExists act1c <> exists (InvestigatorAt $ be a))
      $ forced
      $ RoundEnds #when

instance RunMessage SportsField_172 where
  runMessage msg l@(SportsField_172 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ be attrs) \iid -> do
        sid <- getRandom
        beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid (attrs.ability 1) 2
      pure l
    _ -> SportsField_172 <$> liftRunMessage msg attrs
