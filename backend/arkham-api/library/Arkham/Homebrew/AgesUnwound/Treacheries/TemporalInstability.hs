module Arkham.Homebrew.AgesUnwound.Treacheries.TemporalInstability (temporalInstability) where

import Arkham.Ability
import Arkham.Helpers.Investigator (getSkillValue)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Treachery.Import.Lifted

newtype TemporalInstability = TemporalInstability TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporalInstability :: TreacheryCard TemporalInstability
temporalInstability = treachery TemporalInstability Cards.temporalInstability

{- | "Forced - After you fail a skill test: After this test ends, place 1 of your
clues on your location, then move once towards Sports Field. /
Forced - At the end of the round: Test your lowest skill (2), then discard
Temporal Instability."

The harsher sibling of @night_of_fire@'s /Temporal Stutter/, which discards
itself at the end of the round for free; this one charges a test first.
-}
instance HasAbilities TemporalInstability where
  getAbilities (TemporalInstability a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ SkillTestResult #after You AnySkillTest #failure
    , restricted a 2 (InThreatAreaOf You) $ forced $ RoundEnds #when
    ]

instance RunMessage TemporalInstability where
  runMessage msg t@(TemporalInstability attrs) = runQueueT $ case msg of
    -- "Revelation - Add Temporal Instability to your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> afterThisTestResolves sid do
        placeCluesOnLocation iid (attrs.ability 1) 1
        {- Matched by title rather than by def: Scenario VI gathers its own Sports
        Field (@:ages-unwound:172@) alongside @night_of_the_ritual@, and the
        scenario's entry costs are title-matched for the same reason. -}
        moveTowardsMatching (attrs.ability 1) iid "Sports Field"
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Test your lowest skill (2)" -- all skills tied for lowest are offered,
      -- the way Lair of Dagon's Forced does it.
      lowest <- mins <$> traverse (traverseToSnd (`getSkillValue` iid)) [minBound .. maxBound]
      sid <- getRandom
      chooseOrRunOneM iid $ for_ lowest \skill ->
        skillLabeled skill $ beginSkillTest sid iid (attrs.ability 2) iid skill (Fixed 2)
      pure t
    -- "then discard Temporal Instability" -- unconditionally, pass or fail.
    SkillTestEnds _ _ (isAbilitySource attrs 2 -> True) -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> TemporalInstability <$> liftRunMessage msg attrs
