module Arkham.Homebrew.AgesUnwound.Acts.BreakingTheCircle (breakingTheCircle) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (recordTheTimeFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype BreakingTheCircle = BreakingTheCircle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingTheCircle :: ActCard BreakingTheCircle
breakingTheCircle = act (3, A) BreakingTheCircle Cards.breakingTheCircle Nothing

{- | "Objective - If Ritual Circle has no clues on it, you must immediately
advance."

Matched by title: whichever of the two Ritual Circles act 2 put into play is the
one in play, and both are printed "Ritual Circle". @LocationWithoutClues@ also
matches an unrevealed circle, which is correct -- an unrevealed location holds no
clues, and the act's own wording is about the clues, not the face.
-}
instance HasAbilities BreakingTheCircle where
  getAbilities (BreakingTheCircle a) =
    [ restricted a 1 (exists $ LocationWithTitle "Ritual Circle" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BreakingTheCircle where
  runMessage msg a@(BreakingTheCircle attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      {- "In your Campaign Log, record that the investigators broke the first
      circle. Next to this, record the time. (->R3)."

      The time is read here rather than in the resolution: an agenda is still in
      play at this point, and resolution 3 would otherwise be comparing against
      'getTheTime''s no-agenda (3,4) fallback. -}
      record TheInvestigatorsBrokeTheFirstCircle
      recordTheTimeFor TheInvestigatorsBrokeTheFirstCircle
      push R3
      pure a
    _ -> BreakingTheCircle <$> liftRunMessage msg attrs
