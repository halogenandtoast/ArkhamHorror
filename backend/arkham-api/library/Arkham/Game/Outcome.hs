{-# LANGUAGE TemplateHaskell #-}

{- | How a finished game ended.

'Arkham.Game.State.GameState' has one terminal constructor, 'IsOver', so a won
campaign and a wiped party were indistinguishable once the game stopped. This
module adds the verdict, plus the ending that produced it, so a finished game can
say which it was.

Two provenances, kept apart on purpose. 'Recorded' is a verdict the ending itself
declared, by pushing 'Arkham.Message.GameOverWith'. 'Inferred' is one
reconstructed afterwards by 'inferCampaignOutcome' from what the campaign left
behind, which is all that is available for a game that finished before any of
this existed. The two must never be confused: the first is the campaign guide
speaking, the second is a rule of thumb about how campaigns are wired.
-}
module Arkham.Game.Outcome where

import Arkham.CampaignStep
import Arkham.Id
import Arkham.Json
import Arkham.Prelude
import Arkham.Resolution
import Data.Aeson.TH
import Data.Map.Strict qualified as Map

{- | The coarse verdict. Arkham is full of bittersweet endings, so this is the
roll-up; 'outcomeEnding' carries which ending it actually was.
-}
data GameResult = Won | Lost
  deriving stock (Ord, Eq, Show, Data)

$(deriveJSON defaultOptions ''GameResult)

{- | How firmly the ending states its verdict.

Into the Maelstrom's resolutions 6 and 7 print "The investigators win the
campaign\8230?" -- a win the Campaign Guide itself will not commit to.
'Uncertain' keeps that apart from a win stated outright, so it can be counted
and shown as the thing it is instead of being rounded to either side.
-}
data Certainty = Certain | Uncertain
  deriving stock (Ord, Eq, Show, Data)

$(deriveJSON defaultOptions ''Certainty)

{- | What an ending says the investigators did.

'verdictNote' is an i18n key for the clause the guide attaches to the verdict --
Where the Gods Dwell 4 wins the campaign "\8230and are never seen or heard from
in the waking world again". The printed words belong in the locale files, not
here, so what travels is the key.
-}
data Verdict = Verdict
  { verdictResult :: GameResult
  , verdictCertainty :: Certainty
  , verdictNote :: Maybe Text
  }
  deriving stock (Ord, Eq, Show, Data)

$(deriveJSON (aesonOptions $ Just "verdict") ''Verdict)

-- | The common case: an ending that states its verdict plainly.
plainly :: GameResult -> Verdict
plainly r = Verdict r Certain Nothing

{- | Whether the ending declared the verdict or it was reconstructed. Anything
presenting an outcome should be able to say which, because an 'Inferred' one can
be wrong in ways a 'Recorded' one cannot.
-}
data OutcomeSource = Recorded | Inferred
  deriving stock (Ord, Eq, Show, Data)

$(deriveJSON defaultOptions ''OutcomeSource)

{- | The ending a game stopped on: the last scenario played and the resolution it
reached. 'NoResolution' is a scenario lost or resigned out of.
-}
data Ending = Ending
  { endingScenario :: ScenarioId
  , endingResolution :: Resolution
  }
  deriving stock (Ord, Eq, Show, Data)

$(deriveJSON (aesonOptions $ Just "ending") ''Ending)

data GameOutcome = GameOutcome
  { outcomeVerdict :: Verdict
  , outcomeEnding :: Maybe Ending
  -- ^ Absent only when the game stopped somewhere no scenario had resolved.
  , outcomeSource :: OutcomeSource
  }
  deriving stock (Ord, Eq, Show, Data)

{- | Hand-written so the verdict's fields sit at the top level rather than under
a @verdict@ key. Readers are a materialized view extracting jsonb paths, a
backfill, and three frontend surfaces; a flat @result@/@certainty@/@note@ is one
path each for all of them, and 'Verdict' stays a single type on this side.
-}
instance ToJSON GameOutcome where
  toJSON o =
    object
      [ "result" .= verdictResult (outcomeVerdict o)
      , "certainty" .= verdictCertainty (outcomeVerdict o)
      , "note" .= verdictNote (outcomeVerdict o)
      , "ending" .= outcomeEnding o
      , "source" .= outcomeSource o
      ]

instance FromJSON GameOutcome where
  parseJSON = withObject "GameOutcome" \o -> do
    verdict <-
      Verdict
        <$> o
        .: "result"
        -- Outcomes recorded before uncertainty was expressible stated the
        -- verdict plainly, which is what their absence means.
        <*> o
        .:? "certainty"
        .!= Certain
        <*> o
        .:? "note"
    GameOutcome verdict <$> o .:? "ending" <*> o .: "source"

-- | The verdict's coarse half, for the many readers that only want won or lost.
outcomeResult :: GameOutcome -> GameResult
outcomeResult = verdictResult . outcomeVerdict

{- | Campaigns whose winning ending is an 'EpilogueStep'.

Every campaign puts its losing endings on a bare @gameOver@ from the resolution
and routes the win through the epilogue, which is the invariant
'inferCampaignOutcome' leans on, and which the achievement modules already rely
on to detect campaign completion ("the epilogue pushes GameOver itself, and the
losing endings never reach it, so arriving here is both completed and won").

These four have no epilogue at all, so for them reaching any resolution of the
final scenario is the ending, and only 'NoResolution' is a loss. Night of the
Zealot is the clearest case: all three of The Devourer Below's resolutions are
survivals, and only succumbing to Umôrdhoth's terrible vengeance is not.

Derived mechanically -- the campaigns under @Arkham.Campaign.Campaigns@ with no
@EpilogueStep@ anywhere in their module or their achievements -- so it is
checkable rather than remembered.
-}
campaignHasEpilogue :: CampaignId -> Bool
campaignHasEpilogue cid = cid `notElem` noEpilogueCampaigns

{- | See 'campaignHasEpilogue'. Night of the Zealot and its Return to, plus the
side-story campaigns.

The ids are the ones the campaigns construct themselves -- see the @campaign@
call in each module under @Arkham.Campaign.Campaigns@ -- not their product
codes, which differ.
-}
noEpilogueCampaigns :: [CampaignId]
noEpilogueCampaigns =
  [ "01" -- Night of the Zealot
  , "50" -- Return to the Night of the Zealot
  , "83" -- Guardians of the Abyss
  , "12" -- Brethren of Ash
  ]

{- | Endings that finish a campaign WITHOUT passing through an epilogue, and what
the Campaign Guide says each one is.

These are the exceptions to the rule 'inferCampaignOutcome' leans on, which would
otherwise read every one of them as a loss:

* Shattered Aeons 1, 2 and 3 -- the three ways The Forgotten Age ends at the
  scenario itself. Only resolution 5 walks on to the epilogue (and from there to
  Turn Back Time), and only resolution 4 is the loss.
* Turn Back Time 1 -- @TheInvestigatorsSealedTheRelicOfAgesForever@, how The
  Forgotten Age is won after that epilogue.
* Where the Gods Dwell 3 and 4, Weaver of the Cosmos 3, 4 and 5 -- all reached
  from those scenarios' winning resolutions, and the end of a Dream-Quest or Web
  of Dreams played as a four-part campaign. Two of them win with a clause
  attached, which is what 'verdictNote' carries.
* Into the Maelstrom 6 and 7 -- "The investigators win the campaign\8230?", hence
  'Uncertain'.

Return to the Forgotten Age reaches the first two through its own scenario ids,
so both printings are listed.

Of these, only Into the Maelstrom and the Dream-Eaters endings record a verdict
of their own; the rest reach 'Arkham.Message.GameOver' with nothing stated and
are read off this list every time, not only for games that predate recorded
outcomes.

Deliberately NOT a catalogue of every ending: only the ones whose verdict is
settled. An ending absent from here falls to the rule below.
-}
endingsWithoutEpilogue :: [(ScenarioId, Int, Verdict)]
endingsWithoutEpilogue =
  [ ("04314", 1, plainly Won) -- Shattered Aeons: the tear in time is mended
  , ("04314", 2, plainly Won) -- ...the civilization of the Serpents is saved
  , ("04314", 3, plainly Won) -- ...the civilization of the Yithians is saved
  , ("53061", 1, plainly Won) -- Return to Shattered Aeons, the same three
  , ("53061", 2, plainly Won)
  , ("53061", 3, plainly Won)
  , ("04344", 1, plainly Won) -- Turn Back Time
  , ("53066", 1, plainly Won) -- Return to Turn Back Time
  , ("06286", 3, plainly Won) -- Where the Gods Dwell: the dreamers awoke
  , ("06286", 4, Verdict Won Certain (Just neverSeenInTheWakingWorld))
  , ("06333", 3, plainly Won) -- Weaver of the Cosmos: returned to reality
  , ("06333", 4, Verdict Won Certain (Just neverSeenOrHeardFromAgain))
  , ("06333", 5, plainly Won)
  , ("07311", 6, Verdict Won Uncertain Nothing) -- Into the Maelstrom
  , ("07311", 7, Verdict Won Uncertain Nothing)
  ]

-- | "\8230and are never seen or heard from in the waking world again."
neverSeenInTheWakingWorld :: Text
neverSeenInTheWakingWorld = "outcomeNote.neverSeenInTheWakingWorld"

-- | "\8230but are never seen or heard from again."
neverSeenOrHeardFromAgain :: Text
neverSeenOrHeardFromAgain = "outcomeNote.neverSeenOrHeardFromAgain"

{- | Reconstruct a campaign's outcome from the state it left behind.

The step a campaign stopped on says which ending it took. A loss calls
@gameOver@ from inside the resolution, so 'EndOfScenario' never runs, the step is
still the scenario and it was never added to the completed steps. A win walks on
to the epilogue, so the step is 'EpilogueStep' and the final scenario is the most
recent one in the completed steps. See 'campaignHasEpilogue' for the campaigns
where that second half does not apply.

'Nothing' when there is not enough to judge on -- a campaign abandoned between
scenarios, with nothing resolved.
-}
inferCampaignOutcome
  :: CampaignId
  -- ^ the campaign, for 'campaignHasEpilogue'
  -> CampaignStep
  -- ^ the step it stopped on
  -> [CampaignStep]
  -- ^ its completed steps, most recent first
  -> Map ScenarioId Resolution
  -- ^ every scenario it resolved
  -> Maybe GameOutcome
inferCampaignOutcome cid step completed resolutions
  | step.normalize == EpilogueStep = Just $ GameOutcome (plainly Won) ending Inferred
  | otherwise = case ending of
      Nothing -> Nothing
      Just (Ending _ NoResolution) -> Just $ GameOutcome (plainly Lost) ending Inferred
      Just (Ending sid (Resolution n))
        | verdict : _ <- [v | (s', n', v) <- endingsWithoutEpilogue, s' == sid, n' == n] ->
            Just $ GameOutcome verdict ending Inferred
      Just _ ->
        -- A resolution was reached but the epilogue was not. For a campaign that
        -- has one, that is a losing ending; for one that does not, it IS the
        -- ending.
        Just
          $ GameOutcome
            (plainly $ if campaignHasEpilogue cid then Lost else Won)
            ending
            Inferred
 where
  ending = do
    sid <- finalScenario
    Ending sid <$> Map.lookup sid resolutions
  finalScenario = step.scenario <|> listToMaybe (mapMaybe (.scenario) completed)

{- | A standalone scenario's outcome, which needs no heuristic: the resolution is
the ending.
-}
standaloneOutcome :: Ending -> GameOutcome
standaloneOutcome ending =
  GameOutcome
    (plainly $ if endingResolution ending == NoResolution then Lost else Won)
    (Just ending)
    Recorded
