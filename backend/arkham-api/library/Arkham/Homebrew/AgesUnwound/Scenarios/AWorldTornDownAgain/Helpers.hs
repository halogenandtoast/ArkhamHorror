{- | Scenario-local helpers for Scenario VI, A World Torn Down, Again.

The scenario runs __two act decks__: deck 1 is the @a/b@ deck (the present
investigators) and deck 2 is the @c/d@ deck (their past selves). Deck 2 must
stay deck 2 --- @night_of_the_ritual@'s /Backfire/ advances
@selectOne (ActWithDeckId 1)@ by hand precisely because the three "current act"
helpers (@Scenario.Types.scenarioActs@, @Game.getRemainingActsMatching@,
@Helpers.Act.getCurrentActStep@) all @error@ with two act decks in play.

Anything in this scenario that reads "the current act" must name its deck id.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers where

import Arkham.Ability
import Arkham.Act.Sequence (ActSide (C))
import Arkham.Act.Types (ActAttrs, Field (ActDeckId), onSide)
import Arkham.Card.CardCode (toCardCode)
import Arkham.Card.CardDef (CardDef)
import Arkham.Classes.HasGame
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, selectOne)
import Arkham.Helpers.Act (getActStep)
import Arkham.Helpers.Modifiers (ModifierType (ScenarioModifier), modifySelfWhenM)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.Helpers qualified as AgesUnwound
import Arkham.Homebrew.AgesUnwound.Key (AgesUnwoundKey)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message (AdvancementMethod (AdvancedWithOther), Message (AdvanceAct))
import Arkham.Message.Lifted (advanceToAct)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source (toSource)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = AgesUnwound.scenarioI18n "aWorldTornDownAgain" a

-- | The @a/b@ deck: the present investigators. Hardcoded as deck 1 by /Backfire/.
presentDeck :: Int
presentDeck = 1

-- | The @c/d@ deck: the investigators' past selves.
pastDeck :: Int
pastDeck = 2

{- | The act an act deck currently has in play.

The named-deck replacement for @Helpers.Act.getCurrentActStep@, whose
@selectJust AnyAct@ throws the moment a second act deck exists.
-}
getActStepInDeck :: HasGame m => Int -> m (Maybe Int)
getActStepInDeck n = selectOne (ActWithDeckId n) >>= traverse getActStep

{- | The act matcher for one specific act card.

'Arkham.Matcher.ActMatcher' has no 'Semigroup' instance, so there is no way to
say "step 1 of deck 2" as a single matcher. An 'ActId' /is/ the act's card code
(@Game/Runner.hs:1689@, @Act/Types.hs:195@), so naming the card is both exact
and the only available conjunction.
-}
theAct :: CardDef -> ActMatcher
theAct = ActWithId . ActId . toCardCode

{- | "if act 1c is in play" --- printed on act 2a /The Boundary/, Front Gates and
Sports Field.

Each stage of the past deck is a distinct card, so "act 1c" is exactly /What
Came Before/ being in play; side D is transient (it advances away in the same
batch that flips it).
-}
act1c :: ActMatcher
act1c = theAct Acts.whatCameBefore

{- | How many act decks are in play --- "if only one act deck is in play", printed
on both /Preserve Causality/ and /You Must Not Be Seen/.

Counted off the acts actually in play, so act 3d /All Caught Up/ removing the
remainder of the past deck from the game takes the count back to one and both
treacheries go back to surging. That is the intent: with your past self gone
there is no causality left to preserve.

The Forgotten Age's @getActDecksInPlayCount@ counts by act /side/, which cannot
work here --- this scenario's past deck uses sides C\/D only.
-}
getActDecksInPlay :: HasGame m => m Int
getActDecksInPlay = do
  acts <- select AnyAct
  length . nub <$> traverse (field ActDeckId) acts

{- | "advance to Act 3d" --- straight past act 3c's front to its back, which
removes the remainder of the past deck from the game. Both stage-2 past acts can
be told to do this.

'AdvanceToAct' only auto-advances the act it puts into play when the requested
side is @B@ (@Scenario/Runner.hs:421@), so side D has to be advanced by hand. An
'Arkham.Id.ActId' /is/ the act's card code, which is exactly how the engine's own
@AdvanceToAct@ derives the id it would have advanced.
-}
advanceToActThreeD :: ReverseQueue m => ActAttrs -> m ()
advanceToActThreeD attrs = do
  advanceToAct attrs Acts.theFirstCircle C
  push $ AdvanceAct (ActId $ toCardCode Acts.theFirstCircle) (toSource attrs) AdvancedWithOther

{- | Publish the recorded-time requirement for the corresponding Objective.
Abilities can then test eligibility before offering the Forced prompt.
-}
pastObjectiveModifier :: HasModifiersM m => ActAttrs -> AgesUnwoundKey -> m ()
pastObjectiveModifier attrs key =
  when (onSide C attrs)
    $ modifySelfWhenM attrs (AgesUnwound.isAtOrPastFor key) [ScenarioModifier (tshow key)]

pastObjective :: ActAttrs -> Int -> AgesUnwoundKey -> Ability
pastObjective attrs abilityIndex key =
  onlyOnce
    $ restricted attrs abilityIndex (ActExists $ ActWithModifier $ ScenarioModifier $ tshow key)
    $ Objective
    $ forced
    $ MythosStep AfterCheckDoomThreshold
