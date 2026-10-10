module Arkham.Homebrew.AgesUnwound.Locations.TodayAThousandTimes_196 (
  todayAThousandTimes_196,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted hiding (SkillTestEnded)
import Arkham.Location.Types (revealedL)
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype TodayAThousandTimes_196 = TodayAThousandTimes_196 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

todayAThousandTimes_196 :: LocationCard TodayAThousandTimes_196
todayAThousandTimes_196 =
  symbolLabel
    $ locationWith TodayAThousandTimes_196 Cards.todayAThousandTimes_196 4 (PerPlayer 2)
    $ revealedL
    .~ True

{- | "__Forced__ - After a skill test is performed at this location: Perform that
test again. (Limit once per phase.)"

'Arkham.Message.RepeatSkillTest' off the 'Window.SkillTestEnded' window, which is
how /Live and Learn/ attempts a finished test again -- not
'Arkham.Message.RerunSkillTest', which rewinds a test that is still resolving.

The printed limit is what stops the repeat from repeating itself: the second test
ends in the same phase, so the ability has already been spent. It is a
'groupLimit' rather than a 'playerLimit' because the limit is unqualified.
-}
instance HasAbilities TodayAThousandTimes_196 where
  getAbilities (TodayAThousandTimes_196 a) =
    extendRevealed1 a
      $ groupLimit PerPhase
      $ mkAbility a 1
      $ forced
      $ SkillTestEnded #after (at_ (be a)) AnySkillTest

instance RunMessage TodayAThousandTimes_196 where
  runMessage msg l@(TodayAThousandTimes_196 attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ [st | (windowType -> Window.SkillTestEnded st) <- ws] \st -> do
        sid <- getRandom
        push $ RepeatSkillTest sid st.id
      pure l
    _ -> TodayAThousandTimes_196 <$> liftRunMessage msg attrs
