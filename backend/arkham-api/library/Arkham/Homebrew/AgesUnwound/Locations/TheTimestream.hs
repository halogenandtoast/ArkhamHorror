module Arkham.Homebrew.AgesUnwound.Locations.TheTimestream (theTimestream) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CanCommitToSkillTestPerformedByAnInvestigatorAt))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Homebrew.AgesUnwound.Traits (pattern Adrift)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TheTimestream = TheTimestream LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The bridge home. Set aside at setup and put into play by act 1b, so it is
not part of the ring: its printed Square badge connects to [Diamond, Triangle],
which is every Adrift location plus Arkham, Massachusetts. The Adrift locations
print no symbols, so that connection is one-way by design -- act 2's move
ability is how you get back onto the bridge.
-}
theTimestream :: LocationCard TheTimestream
theTimestream = location TheTimestream Cards.theTimestream 2 (PerPlayer 2)

{- | "[reaction] During a skill test at an [[Adrift]] location, spend 1 clue: You
may commit a card to this skill test." / "Forced - At the end of your turn, if
there is at least one clue on The Timestream: Test [agility] (3). If you fail,
place one of your clues on your location."

The reaction's window is the commit step (ST.2); "a skill test at an [[Adrift]]
location" is a constraint on the /testing/ investigator, so it rides the
window's @Who@ rather than 'SkillTestAt', which asks where the test's target is.
-}
instance HasAbilities TheTimestream where
  getAbilities (TheTimestream a) =
    extendRevealed
      a
      [ restricted a endOfTurnAbility (Here <> thisExists a LocationWithAnyClues)
          $ forced
          $ TurnEnds #when You
      , restricted a 2 Here
          $ triggered
            (CommittingCardsFromHandToSkillTestStep #when (at_ $ LocationWithTrait Adrift))
            (ClueCost $ Static 1)
      ]

instance RunMessage TheTimestream where
  runMessage msg l@(TheTimestream attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ InvestigatorPlaceCluesOnLocation iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      {- The ability only lifts the "you may only commit to your own test"
      restriction; which cards qualify is still the engine's commit check. The
      modifier lands before 'CommitToSkillTest' builds its ask, so the card
      appears in the normal commit prompt. -}
      withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 2) iid
          $ CanCommitToSkillTestPerformedByAnInvestigatorAt (LocationWithTrait Adrift)
      pure l
    _ -> TheTimestream <$> liftRunMessage msg attrs
