module Arkham.Homebrew.AgesUnwound.Acts.BreakingTheCircles (breakingTheCircles) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype BreakingTheCircles = BreakingTheCircles ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingTheCircles :: ActCard BreakingTheCircles
breakingTheCircles = act (4, A) BreakingTheCircles Cards.breakingTheCircles Nothing

{- | "Objective - If each Ritual Circle location is revealed and has no clues on
it, you must immediately advance."

Matched by title: the three printings (@:ages-unwound:173@, @:224@ and @:225@) are
all printed "Ritual Circle", and exactly two are in play -- setup removes the one
the investigators stepped into a year ago. Stated as "none of them is unrevealed
or holds a clue", with an existence guard so the act cannot advance before the
circles are down.
-}
instance HasAbilities BreakingTheCircles where
  getAbilities (BreakingTheCircles a) =
    [ restricted
        a
        1
        ( exists ritualCircles
            <> not_ (exists $ ritualCircles <> oneOf [UnrevealedLocation, LocationWithAnyClues])
        )
        $ Objective
        $ forced AnyWindow
    ]

ritualCircles :: LocationMatcher
ritualCircles = LocationWithTitle "Ritual Circle"

instance RunMessage BreakingTheCircles where
  runMessage msg a@(BreakingTheCircles attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    -- "Glimpse of Things to Come: (->R1)."
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> BreakingTheCircles <$> liftRunMessage msg attrs
