module Arkham.Homebrew.AgesUnwound.Locations.MillionsOfYearsAgo_081 (millionsOfYearsAgo_081) where

import Arkham.Ability
import Arkham.GameEnv (getHistoryField)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.History
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MillionsOfYearsAgo_081 = MillionsOfYearsAgo_081 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Millions of Years Ago/ -- the Cretaceous. Shroud 1 and two clues per
investigator, paid for with a combat test every turn.
-}
millionsOfYearsAgo_081 :: LocationCard MillionsOfYearsAgo_081
millionsOfYearsAgo_081 =
  locationWith MillionsOfYearsAgo_081 Cards.millionsOfYearsAgo_081 1 (PerPlayer 2)
    $ connectsToL
    .~ ringConnections

-- | "Enemies at Millions of Years Ago get +1 fight and -1 evade."
instance HasModifiersFor MillionsOfYearsAgo_081 where
  getModifiersFor (MillionsOfYearsAgo_081 a) =
    modifySelect a (EnemyAt $ be a) [EnemyFight 1, EnemyEvade (-1)]

{- | "Forced - At the end of your turn: Test [combat] (3). If you fail, take X
damage, where X is the number of clues you discovered this turn (min 1)."
-}
instance HasAbilities MillionsOfYearsAgo_081 where
  getAbilities (MillionsOfYearsAgo_081 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage MillionsOfYearsAgo_081 where
  runMessage msg l@(MillionsOfYearsAgo_081 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #combat (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- Clues discovered *anywhere* this turn, not just here: the card does not
      -- scope it to a location.
      discovered <- sum . toList <$> getHistoryField TurnHistory iid HistoryCluesDiscovered
      assignDamage iid (attrs.ability 1) (max 1 discovered)
      pure l
    _ -> MillionsOfYearsAgo_081 <$> liftRunMessage msg attrs
