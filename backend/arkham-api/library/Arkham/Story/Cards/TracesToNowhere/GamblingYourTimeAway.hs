module Arkham.Story.Cards.TracesToNowhere.GamblingYourTimeAway (gamblingYourTimeAway) where

import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (getScenarioMetaKeyDefault)
import Arkham.I18n
import Arkham.Id
import Arkham.Location.CardDefs.TracesToNowhere qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Story.Import.Lifted
import Data.Aeson (Result (..))
import Data.Aeson.Key qualified as Key

newtype GamblingYourTimeAway = GamblingYourTimeAway StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gamblingYourTimeAway :: StoryCard GamblingYourTimeAway
gamblingYourTimeAway = story GamblingYourTimeAway Cards.gamblingYourTimeAway

{- | A hand's running total has to survive the chaos-token request round trip, so
it lives in scenario meta under a per-investigator key rather than being threaded
through the message.
-}
handKey :: InvestigatorId -> Key.Key
handKey iid = Key.fromText $ "tracesToNowhere.blackjack." <> tshow iid

getHand :: HasGame m => InvestigatorId -> m Int
getHand iid = getScenarioMetaKeyDefault (handKey iid) 0

setHand :: ReverseQueue m => InvestigatorId -> Int -> m ()
setHand iid = setGlobal ScenarioTarget (handKey iid)

{- | The loop's own anchor. A hand runs hit-by-hit across several messages, and
this is one the story can both raise and match on -- the story-resolution patterns
are non-bidirectional, so they cannot be constructed to continue the loop.
-}
dealKey :: Text
dealKey = "tracesToNowhere.blackjack.deal"

instance RunMessage GamblingYourTimeAway where
  runMessage msg s@(GamblingYourTimeAway attrs) = runQueueT $ scenarioI18n $ case msg of
    -- "Each investigator at Tick-Tock Club may spend 2 resources to play blackjack."
    ResolveThisStory _ (is attrs -> True) -> do
      players <-
        select
          $ investigatorAt (locationIs Locations.tickTockClub)
          <> InvestigatorWithResources (atLeast 2)
      for_ players \iid -> chooseOneM iid do
        labeled "blackjack.decline" nothing
        labeled "blackjack.play" do
          spendResources iid 2
          setHand iid 0
          scenarioSpecific dealKey iid
      pure s
    -- "...reveals random tokens from the chaos bag until they choose to stop."
    ScenarioSpecific ((== dealKey) -> True) (fromJSON -> Success iid) -> do
      total <- getHand iid
      chooseOneM iid do
        countVar total $ labeled "blackjack.stand" $ settle iid total
        labeled "blackjack.hit" $ requestChaosTokens_ iid (attrs.ability 1) 1
      pure s
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      resetChaosTokens (attrs.ability 1)
      total <- getHand iid
      case concatMap (blackjackValue . (.face)) tokens of
        -- The elder sign is "either a 1 or an 11", so its owner picks.
        [low, high] -> chooseOneM iid do
          countVar low $ labeled "blackjack.countAs" $ advance iid (total + low)
          countVar high $ labeled "blackjack.countAs" $ advance iid (total + high)
        values -> advance iid (total + sum values)
      pure s
    _ -> GamblingYourTimeAway <$> liftRunMessage msg attrs
   where
    -- "...22 or more, you bust and gain nothing."
    advance iid total = do
      setHand iid total
      when (total <= 21) $ scenarioSpecific dealKey iid

    settle iid total = for_ (blackjackPayout total) $ gainResources iid (attrs.ability 1)
