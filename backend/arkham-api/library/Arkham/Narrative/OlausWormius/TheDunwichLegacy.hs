{-# LANGUAGE ImplicitParams #-}

{- | "The Dunwich Legacy of Olaus Wormius" -- a community rewrite of the
campaign's prose by Olaus Wormius.

The rewrite changes nothing mechanically, so everything here is narrative. A
block it only rewords needs nothing in this module: the client resolves the key
the engine already emits against the variant's messages. What lives here is the
rest -- the blocks it splits by the Lead Investigator's identity, the ones it
builds from several pieces, and the interlude it adds between Scenarios II and
III.

The beats are gathered into 'narrative', the record the campaign reads them off.
The campaign's own code never names this rewrite -- only the registry does.
-}
module Arkham.Narrative.OlausWormius.TheDunwichLegacy where

import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey (recorded)
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card.CardCode
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Campaign (getCompletedScenarios)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Narrative
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Narrative.TheDunwichLegacy.Types
import Arkham.Prelude
import Arkham.Scenario.Helpers (getHasRecordOrStandalone)
import Arkham.Trait

{- | The name this rewrite is selected and keyed under. Registered against it in
"Arkham.Narrative.TheDunwichLegacy".
-}
variant :: Text
variant = "olausWormius"

{- | The beats this rewrite changes. Everything it only rewords resolves from its
own messages at the key the engine already emits, so it is absent here.
-}
narrative :: (HasI18n, ReverseQueue m) => DunwichNarrative m
narrative =
  noDunwichNarrative
    { prologue = Just prologue
    , armitagesFate1 = Just armitagesFate1
    , interlude1a = Just interlude1a
    , interlude2Armitage = Just interlude2Armitage
    , interlude2FrancisMorgan = Just interlude2FrancisMorgan
    , epilogue = Just epilogue
    , extracurricularActivityIntro = Just extracurricularActivityIntro
    , extracurricularActivityResolution2 = Just extracurricularActivityResolution2
    , theHouseAlwaysWinsIntro = Just theHouseAlwaysWinsIntro
    , theHouseAlwaysWinsResolution3 = Just theHouseAlwaysWinsResolution3
    , theMiskatonicMuseumIntro = Just theMiskatonicMuseumIntro
    , theMiskatonicMuseumIntroPart3 = Just theMiskatonicMuseumIntroPart3
    , theMiskatonicMuseumResolution1 = Just theMiskatonicMuseumResolution1
    , theMiskatonicMuseumResolution2 = Just theMiskatonicMuseumResolution2
    , theEssexCountyExpressIntro = Just theEssexCountyExpressIntro
    , theEssexCountyExpressResolution1 = Just theEssexCountyExpressResolution1
    , bloodOnTheAltarIntro = Just bloodOnTheAltarIntro
    , bloodOnTheAltarResolution2 = Just bloodOnTheAltarResolution2
    , undimensionedAndUnseenIntro = Just undimensionedAndUnseenIntro
    , undimensionedAndUnseenPart1 = Just undimensionedAndUnseenPart1
    , undimensionedAndUnseenPart2 = Just undimensionedAndUnseenPart2
    , whereDoomAwaitsIntro = Just whereDoomAwaitsIntro
    , whereDoomAwaitsResolution1 = Just whereDoomAwaitsResolution1
    , lostInTimeAndSpaceIntro = Just lostInTimeAndSpaceIntro
    }

-- * Prologue

{- | Five framings of the encounter on College Street, by who the Lead
Investigator is, then the conversation they all run into.
-}
prologue :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
prologue = do
  branch <-
    firstBranch
      [ (leadHasTrait Miskatonic, "miskatonic")
      , (leadIsClass Mystic, "mystic")
      , (leadIsNamed ["Charlie Kane"], "charlieKane")
      , (orM [leadIsClass Seeker, leadIsNamed ["Kohaku Narukami"]], "seekerOrKohaku")
      ]
  pure do
    setTitle "title"
    p "body"
    p.basic "readOneOfTheFollowing"
    p branch
    p "tail"
    footnote 2 "footnote2"

-- * Interlude I: Armitage's Fate

-- | Mrs. Armitage on the telephone, with the rewrite's note on her existence.
armitagesFate1 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
armitagesFate1 = pure do
  setTitle "title"
  p "armitagesFate1"
  footnote 3 "footnote3"

-- * Interlude II: The Survivors

{- | Armitage greets the investigators differently depending on whether they
pulled him out of the museum or followed him all the way to Dunwich.
-}
interlude2Armitage :: HasGame m => m Scope
interlude2Armitage = do
  rescued <- getHasRecord TheInvestigatorsRescuedDrHenryArmitage
  pure $ if rescued then "drHenryArmitageRescued" else "drHenryArmitageKidnapped"

-- | Morgan on his rifle, with the rewrite's citation for Armitage's warning.
interlude2FrancisMorgan :: (HasI18n, ReverseQueue m) => m ()
interlude2FrancisMorgan = flavor $ footnote 11 "footnote11"

-- * Scenario I-A: Extracurricular Activity

{- | Whether the night started at the Clover Club decides how you arrive. The
Return box gives that scenario its own id, so both count.
-}
extracurricularActivityIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
extracurricularActivityIntro = do
  completedTheHouseAlwaysWins <- any (`elem` ["02062", "51015"]) <$> getCompletedScenarios
  pure do
    h "title"
    p $ if completedTheHouseAlwaysWins then "completedTheHouseAlwaysWins" else "firstScenario"

{- | Resolution 2 is told twice: the dormitory listens differently to a face it
knows from campus.
-}
extracurricularActivityResolution2 :: HasGame m => m Scope
extracurricularActivityResolution2 = do
  miskatonic <- leadHasTrait Miskatonic
  pure $ if miskatonic then "resolution2Miskatonic" else "resolution2NotMiskatonic"

-- * Scenario I-B: The House Always Wins

-- | Four ways the bouncer at the vestibule greets you.
theHouseAlwaysWinsIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
theHouseAlwaysWinsIntro = do
  branch <-
    firstBranch
      [ (leadIsNamed ["Jim Culver", "Marie Lambeau"], "jimOrMarie")
      , (leadHasTrait Criminal, "criminal")
      , (leadIsNamed ["Daniela Reyes", "Nathaniel Cho", "Roland Banks", "Tommy Muldoon"], "daniela")
      ]
  pure do
    h "title"
    p "body"
    p.basic "readOneOfTheFollowing"
    p branch

{- | Resolution 3 adds Naomi's recognition of a public figure before the scene
both readings reach.
-}
theHouseAlwaysWinsResolution3 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
theHouseAlwaysWinsResolution3 = do
  charlieKane <- leadIsNamed ["Charlie Kane"]
  pure do
    setTitle "resolution3.title"
    p "resolution3.body"
    p $ if charlieKane then "resolution3.charlieKane" else "resolution3.otherwise"
    p "resolution3.part3a"
    p.basic "resolution3.consequences"

-- * Scenario II: The Miskatonic Museum

-- | The story of the Dunwich horror, with the rewrite's citation for it.
theMiskatonicMuseumIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
theMiskatonicMuseumIntro = do
  armitageKidnapped <- getHasRecordOrStandalone DrHenryArmitageWasKidnapped True
  pure do
    h "title"
    p "body"
    footnote 4 "footnote4"
    p.validate armitageKidnapped "proceedToPart1"
    p.validate (not armitageKidnapped) "proceedToPart2"

{- | Seth Bishop's letter, which both Part 1 and Part 2 reach. Read from the
intro's own scope, not the part's, because there is one letter.
-}
theMiskatonicMuseumIntroPart3 :: (HasI18n, ReverseQueue m) => m ()
theMiskatonicMuseumIntroPart3 = popScope $ scope "intro" $ flavor (h "title" >> p "part3")

-- | Burning the book, as a Believer or Warden sees it or as anyone else does.
theMiskatonicMuseumResolution1 :: HasGame m => m Scope
theMiskatonicMuseumResolution1 = do
  believerOrWarden <- orM [leadHasTrait Believer, leadHasTrait Warden]
  pure $ if believerOrWarden then "resolution1BelieverOrWarden" else "resolution1Otherwise"

-- | Keeping the book, as a Sorcerer or Miskatonic scholar sees it or otherwise.
theMiskatonicMuseumResolution2 :: HasGame m => m Scope
theMiskatonicMuseumResolution2 = do
  sorcererOrMiskatonic <- orM [leadHasTrait Sorcerer, leadHasTrait Miskatonic]
  pure $ if sorcererOrMiskatonic then "resolution2SorcererOrMiskatonic" else "resolution2Otherwise"

-- * Interlude I-A: False Leads

--

{- $interlude1a
The interlude the rewrite adds between Scenarios II and III, so a group that
wants a side scenario has somewhere to put it.
-}

interlude1a :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
interlude1a = do
  rescued <- getHasRecord TheInvestigatorsRescuedDrHenryArmitage
  pure do
    setTitle "title"
    p "body"
    p.basic.validate rescued "proceedToPart1"
    p.basic.validate (not rescued) "proceedToPart2"
    if rescued
      then p "part1" >> footnote 5 "footnote5" >> footnote 6 "footnote6"
      else p "part2"
    p.basic "sideScenario"

-- * Scenario III: The Essex County Express

-- | Two weeks of waiting, told by Armitage himself or by his wife.
theEssexCountyExpressIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
theEssexCountyExpressIntro = do
  rescued <- getHasRecord TheInvestigatorsRescuedDrHenryArmitage
  pure do
    h "title"
    p.basic.validate rescued "proceedToPart1"
    p.basic.validate (not rescued) "proceedToPart2"
    if rescued
      then p "part1" >> footnote 7 "footnote7"
      else p "part2"
    p "part3"

-- | Resolution 1 quotes Armitage's account; the citation follows it.
theEssexCountyExpressResolution1 :: (HasI18n, ReverseQueue m) => m ()
theEssexCountyExpressResolution1 = flavor $ footnote 8 "footnote8"

-- * Scenario IV: Blood on the Altar

-- | The mobsters' arrival in Dunwich, if they followed you here.
bloodOnTheAltarIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
bloodOnTheAltarIntro = do
  oBannion <- getHasRecord OBannionGangHasABoneToPickWithTheInvestigators
  pure do
    h "title"
    p "body"
    footnote 10 "footnote10"
    p.validate oBannion "oBannionGang"

{- | Restoring Silas with the Necronomicon, then the book taking over. Which way
it takes over turns on whether the Lead Investigator's deckbuilding allows
second-level Mystic cards -- a property of the deck's construction, which the
engine does not model, so the question is put to them as the document puts it to
the table.
-}
bloodOnTheAltarResolution2 :: (HasI18n, ReverseQueue m) => m ()
bloodOnTheAltarResolution2 = scope "resolution2" do
  lead <- getLead
  chooseOneM lead do
    labeled "allowsSecondLevelMystic" $ flavor $ p "part2a" >> p "part2c"
    labeled "otherwise" $ flavor $ p "part2b" >> p "part2c"

-- * Scenario V: Undimensioned and Unseen

{- | Curtis on how Seth turned, with Armitage's part in it if he is still alive
to take it, and his note on where the Whateley books went.
-}
undimensionedAndUnseenIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
undimensionedAndUnseenIntro = do
  sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
  let armitageSurvived =
        recorded (toCardCode Assets.drHenryArmitage) `notElem` sacrificedToYogSothoth
  pure do
    h "title"
    p "body"
    when armitageSurvived do
      p "drHenryArmitage"
      footnote 12 "footnote12"
    p "tail"

{- | Calming the townsfolk: a Survivor speaks to them as one of them. Either way
the scene ends with Gerty's account, and with the sprayer if the powder survived.
-}
undimensionedAndUnseenPart1 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
undimensionedAndUnseenPart1 = do
  survivor <- leadIsClass Survivor
  powder <- hasPowderOfIbnGhazi
  pure do
    h "title"
    p $ if survivor then "part1Survivor" else "part1NotSurvivor"
    p "part1a"
    when powder $ p "part1b"

-- | Warning the townsfolk, with a Guardian or Warden naming them a leader.
undimensionedAndUnseenPart2 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
undimensionedAndUnseenPart2 = do
  guardianOrWarden <- orM [leadIsClass Guardian, leadHasTrait Warden]
  powder <- hasPowderOfIbnGhazi
  pure do
    h "title"
    p $ if guardianOrWarden then "part2GuardianOrWarden" else "part2NotGuardianOrWarden"
    when powder $ p "part2a"

hasPowderOfIbnGhazi :: HasGame m => m Bool
hasPowderOfIbnGhazi = selectAny $ InDeckOf Anyone <> basic (cardIs Assets.powderOfIbnGhazi)

-- * Scenario VI: Where Doom Awaits

{- | The drive to Sentinel Hill, with whichever of the survivors is there to
watch the hilltop burn.
-}
whereDoomAwaitsIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
whereDoomAwaitsIntro = do
  sacrificedToYogSothoth <- getRecordSet SacrificedToYogSothoth
  let survived asset = recorded (toCardCode asset) `notElem` sacrificedToYogSothoth
  hasTheInvestigatorsBack <- getHasRecord NaomiHasTheInvestigatorsBacks
  pure do
    h "title"
    p.validate (survived Assets.drHenryArmitage) "drHenryArmitage"
    p.validate (survived Assets.zebulonWhateley) "zebulonWhateley"
    footnote 13 "footnote13"
    p "body"
    p.basic.validate hasTheInvestigatorsBack "proceedToPart1"
    p.basic.validate (not hasTheInvestigatorsBack) "otherwise"

{- | The porter's verse, then the fall through the gate, before Resolution 1's
own consequences.
-}
whereDoomAwaitsResolution1 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
whereDoomAwaitsResolution1 = pure do
  setTitle "resolution1.title"
  p "resolution1.body"
  p.center.noUnderline "resolution1.verse"
  p.center.noUnderline "resolution1.owixa"
  p "resolution1.part1a"
  p.basic "resolution1.consequences"

-- * Scenario VII: Lost in Time and Space

-- | Kadath and John Dee, each with the rewrite's citation.
lostInTimeAndSpaceIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
lostInTimeAndSpaceIntro = pure do
  h "title"
  p "body"
  footnote 14 "footnote14"
  footnote 15 "footnote15"

-- * Epilogue

-- | One epilogue, whatever was decided for the townsfolk.
epilogue :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
epilogue = pure do
  setTitle "title"
  p.basic "intro"
  p "body"
