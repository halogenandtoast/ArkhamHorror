module Arkham.Homebrew.AgainstTheWendigo.Locations.SarceeTerritory (sarceeTerritory) where

import Arkham.Ability
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Homebrew.AgainstTheWendigo.Helpers (civilizedResign)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype SarceeTerritory = SarceeTerritory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sarceeTerritory :: LocationCard SarceeTerritory
sarceeTerritory = location SarceeTerritory Cards.sarceeTerritory 3 (PerPlayer 1)

instance HasAbilities SarceeTerritory where
  getAbilities (SarceeTerritory a) =
    extendRevealed
      a
      [ civilizedResign a
      , {- | "An investigator may attempt one of the following 2 actions
        (Collective limit of once per game)": persuade Imala, or intimidate her.
        Both are Parleys and both reveal the Isolated Land on a success; only the
        second records that the Sarcee are hunting you. The resource spend on the
        first is handled by the test's own difficulty reduction. -}
        groupLimit PerGame $ skillTestAbility $ restricted a 1 Here parleyAction_
      , groupLimit PerGame $ skillTestAbility $ restricted a 2 Here parleyAction_
      ]


instance RunMessage SarceeTerritory where
  runMessage msg l@(SarceeTerritory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 6)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      record TheSarceeAreHuntingYouDown
      parley sid iid (attrs.ability 2) attrs #combat (Fixed 3)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> revealIsolatedLand l
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> revealIsolatedLand l
    _ -> SarceeTerritory <$> liftRunMessage msg attrs
   where
    -- "If you succeed, reveal the Isolated Land."
    revealIsolatedLand l' = do
      selectForMaybeM (locationIs Cards.hiddenHut) reveal
      pure l'
