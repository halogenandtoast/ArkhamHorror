{- | Campaign-log keys owned by the Ages Unwound campaign.

Taken from the campaign log in @docs/homebrew/data/ages-unwound-structure.md@,
grouped by the step that records each one. Keys plug into the core log through
the shared 'Arkham.CampaignLogKey.HomebrewCampaignLogKey' wrapper and serialize
by name; the @agesUnwound.@ prefix lets the frontend read the i18n scope off the
key. Do not add or remove that prefix once a save exists — 'hasRecord' compares
the serialized key.

Each key needs display text under @key.\<camelCaseName\>@ in
@frontend/homebrew/ages-unwound/locales/en/base.json@.
-}
module Arkham.Homebrew.AgesUnwound.Key (module Arkham.Homebrew.AgesUnwound.Key) where

import Arkham.CampaignLogKey (CampaignLogKey (HomebrewCampaignLogKey), IsCampaignLogKey (..))
import Arkham.Prelude

data AgesUnwoundKey
  = -- | Scenario I: Night of Fire
    YourHuntersFoundANewQuarry
  | TheInvestigatorsSurvivedTheNightOfFire
  | TheInvestigatorsSlewTheirStrangeObserver
  | TheInvestigatorsEscapedTheNightOfFire
  | -- | Interlude I: An Unknown Benefactor
    TheInvestigatorsDeclinedAnOfferOfHelp
  | TheInvestigatorsAcceptedAnOfferOfHelp
  | -- | Scenario II: The Myriad Gentleman
    TheInvestigatorsLearntOfTheMyriadsRitual
  | TheInvestigatorsFledTheGentlemansManor
  | {- | Scenario III: A World Torn Down. The first three are recorded /with a
    time/ — see 'Arkham.Homebrew.AgesUnwound.Helpers.recordTheTime', which also
    adds a 'RecordedTime' entry to 'RecordedTimes'.
    -}
    TheBoundaryIsBroken
  | TheInvestigatorsUsedTheSchoolsFrontDoor
  | TheInvestigatorsSnuckIntoTheBackOfTheSchool
  | -- | recorded with the time AND 'DamageOnTheHoundOfUnmaking'
    TheInvestigatorsRepelledTheHoundOfUnmaking
  | -- | recorded with the time
    TheInvestigatorsPutDownTheHoundOfUnmaking
  | -- | recorded with the time
    TheInvestigatorsDispelledTheWard
  | -- | recorded with the time
    TheInvestigatorsBrokeTheFirstCircle
  | {- | A count, not a flag: act 2b records the damage on the Hound when it is
    repelled, and Scenario VI's Hound enters play with that much damage.
    -}
    DamageOnTheHoundOfUnmaking
  | TheInvestigatorsFellToTheMyriad
  | TheInvestigatorsSteppedIntoThePast
  | TheInvestigatorsSteppedIntoTheFuture
  | TheInvestigatorsEliminatedTheHighPriest
  | TheTimelineWasWeakened
  | -- | The losing end of both Scenario III and Scenario VII.
    AllTimesAreOne
  | -- | Scenario V: A Year to Plan
    TheRitualIsNigh
  | -- | recorded per investigator
    ReturnedToArkhamLate
  | TheInvestigatorsViolatedCausality
  | TheMyriadSolvedTheRiddleOfTheSphinx
  | {- | The investigators' own version, recorded by the Ancient Sphinx's
    @Answers@ story back. Distinct from 'TheMyriadSolvedTheRiddleOfTheSphinx',
    which Scenario V records when the task is left incomplete.
    -}
    YouSolvedTheRiddleOfTheSphinx
  | -- | Recorded by Another Realm's @Destination@ story back.
    YouTookTeaWithTheRulerOfAStrangeDimension
  | TheMyriadHarnessedThePowerOfAnotherRealm
  | TheMyriadTookControlOfAColourOutOfSpace
  | TheMyriadRaisedAPowerfulWarding
  | TheMyriadRecruitedACruelSorcerer
  | TheMyriadWeavedADreadCurse
  | {- | Scenario VI: A World Torn Down, Again. A record set of card codes —
    "[enemy] disappeared unexpectedly" names a card, so it is written with
    @recordSetInsert ... [toCardCode def]@ and read with 'getRecordedCardCodes'.
    -}
    DisappearedUnexpectedly
  | TheInvestigatorsUnleashedChaos
  | -- | Scenario VII: Time Runs Out
    TheInvestigatorsBoundAforgomonInAPrisonOfTime
  | -- | recorded per investigator
    StillBearsAforgomonsMark
  | -- | recorded per investigator
    ExistenceIsWaning
  | {- | Written by a /different/ campaign's log and read by Scenario VI's setup
    and the epilogue. Treated as a plain key.
    -}
    YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge
  | {- | Tally: +1 at the Prologue, +1 for Interlude I's "Leap of Faith", +1 per
    /Aid from Afar/ resolution in Scenario III. Scenario V's "Helping
    Yourself" enters play with this many resources.
    -}
    StrangeAssistance
  | {- | Record set of 'Arkham.Homebrew.AgesUnwound.Helpers.RecordedTime' JSON
    entries — the @(agenda number, doom on agenda)@ pairs Scenarios III and VI
    compare against. Written and read only through
    'Arkham.Homebrew.AgesUnwound.Helpers.recordTheTime' / 'isAtOrPast'.
    -}
    RecordedTimes
  | {- | Scenarios III and VII can send the table back to themselves. The
    resolution records this; the scenario's own Setup crosses it out, so
    @nextStep@ reading the uncrossed record cannot loop forever.
    -}
    MustReplayAWorldTornDown
  | MustReplayTimeRunsOut
  deriving stock (Show, Read, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance IsCampaignLogKey AgesUnwoundKey where
  toCampaignLogKey = HomebrewCampaignLogKey . ("agesUnwound." <>) . tshow
  fromCampaignLogKey = \case
    HomebrewCampaignLogKey t ->
      readMay . unpack $ fromMaybe t (stripPrefix "agesUnwound." t)
    _ -> Nothing
