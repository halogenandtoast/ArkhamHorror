module Arkham.Homebrew.AgesUnwound.Locations.HeartOfAnEmpire_079 (heartOfAnEmpire_079) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)

newtype HeartOfAnEmpire_079 = HeartOfAnEmpire_079 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Heart of an Empire/ -- Rome, the printing that throws you back out of it.
heartOfAnEmpire_079 :: LocationCard HeartOfAnEmpire_079
heartOfAnEmpire_079 =
  locationWith HeartOfAnEmpire_079 Cards.heartOfAnEmpire_079 4 (PerPlayer 1)
    $ connectsToL
    .~ ringConnections

{- | "Forced - At the end of your turn: Test [willpower] (3). If you fail, move
to a random other [[Adrift]] location. Do not resolve any forced abilities on
that location that would trigger at the end of your turn."
-}
instance HasAbilities HeartOfAnEmpire_079 where
  getAbilities (HeartOfAnEmpire_079 a) =
    extendRevealed1 a $ restricted a endOfTurnAbility Here $ forced $ TurnEnds #when You

instance RunMessage HeartOfAnEmpire_079 where
  runMessage msg l@(HeartOfAnEmpire_079 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      {- The destination has to be known before the suppression can name it, so
      the random Adrift location is drawn here rather than through
      'moveToRandomOtherAdrift'. -}
      mlid <- getRandomAdriftLocation $ not_ (LocationWithInvestigator $ InvestigatorWithId iid)
      for_ mlid \lid -> do
        ignoreEndOfTurnForcedAt (attrs.ability 1) iid lid
        moveTo (attrs.ability 1) iid lid
      pure l
    _ -> HeartOfAnEmpire_079 <$> liftRunMessage msg attrs
