module Arkham.Event.Events.ButterflyEffect1Spec (spec) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Modifier
import Arkham.Skill.Cards qualified as Skills
import Helpers.Message qualified
import TestImport.New

{- | Sets up a willpower test with @guts@ committed (it is a willpower skill, so
the test has to match), then hands the body the skill test, the committed card,
and Butterfly Effect. The body applies whatever modifier it wants and then calls
'playButterfly'. A spare wild skill stays in hand so the event always has at
least one candidate, even when @guts@ turns out not to be returnable.
-}
setup :: Investigator -> (SkillTestId -> Card -> Card -> TestAppT ()) -> TestAppT ()
setup self body = do
  location <- testLocation
  self `moveTo` location
  setChaosTokens [Skull]
  butterfly <- genCard Cards.butterflyEffect1
  guts <- genMyCard self Skills.guts
  spare <- genMyCard self Skills.unexpectedCourage
  self `addToHand` butterfly
  self `addToHand` guts
  self `addToHand` spare
  sid <- getRandom
  -- Not runSkillTest: it clicks straight through the commit window and we need
  -- to commit into it first.
  run $ Helpers.Message.beginSkillTest sid self #willpower 2
  commit guts
  body sid guts butterfly

{- | Butterfly Effect is fast off a revealed symbol token, so it can only be
played once the test is under way and the Skull has come out.
-}
playButterfly :: Card -> TestAppT ()
playButterfly butterfly = do
  startSkillTest
  chooseTarget butterfly

spec :: Spec
spec = describe "Butterfly Effect (1)" do
  -- MustBeCommitted only stops the investigator taking the commit back during
  -- the commit window. It is applied by every card that commits on your behalf
  -- after a cost was paid (Isabelle Barnes, Surprising Find, Practice Makes
  -- Perfect, ...). Returning a committed card to hand by card effect is a
  -- different thing, so those cards must still be offered here.
  it "offers to return a card whose commit cannot be taken back" . gameTest $ \self -> do
    setup self \sid guts butterfly -> do
      run =<< skillTestModifier sid (TestSource mempty) (toCardId guts) MustBeCommitted
      playButterfly butterfly
      assertTarget (toCardId guts)

  -- LeaveCardWhereItIs marks a card committed from a zone it never physically
  -- left (Amanda Sharpe's top-of-deck card, Dayana Esperence's stashed cards).
  -- Those genuinely cannot be returned to hand.
  it "does not offer a card that never left the zone it was committed from" . gameTest $ \self -> do
    setup self \sid guts butterfly -> do
      run =<< skillTestModifier sid (TestSource mempty) (toCardId guts) LeaveCardWhereItIs
      playButterfly butterfly
      assertNotTarget (toCardId guts)

  -- The card says "may", so declining has to be possible.
  it "can be declined" . gameTest $ \self -> do
    setup self \_ _ butterfly -> do
      playButterfly butterfly
      clickLabel "$label.doNothing"
