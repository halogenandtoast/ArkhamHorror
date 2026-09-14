module Arkham.Homebrew.DarkMatter.Scenarios.Starfall (starfall) where

import Arkham.Card (toCardCode)
import Arkham.Helpers.FlavorText (flavor, h, li, p, resolutionOnly, setTitle, setup, ul)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (addScanningDeck, earnXp, getImpendingDoom, scenarioI18n)
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Id (AgendaId (..), InvestigatorId)
import Arkham.Investigator.Types (Field (InvestigatorMentalTrauma))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype Starfall = Starfall ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Resolution 1 hands the investigators who resigned in Hope\/New Brooklyn a
different ending from everyone else, so the colony they resigned at has to be
remembered while they are still standing on it.
-}
data Meta = Meta
  { resignedInHope :: [InvestigatorId]
  , resignedInNewBrooklyn :: [InvestigatorId]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta [] []

{- | Five content columns — Sol, Earth\/Moon, Mars, the Asteroid Belt and Pluto —
each flanked by a berth column on either side, so every location has an @l\<label\>@
cell to its left and an @r\<label\>@ cell to its right. The two [[Starship]]
locations that attach dock in those: The Tatterdemalion always takes the left
berth of its host, The Cassilda always the right (see their 'HandleTargetChoice'
handlers). @theTatterdemalion@ and @theCassilda@ are where each sits while it is
attached to nothing.
-}
starfall :: Difficulty -> Starfall
starfall difficulty =
  scenario
    Starfall
    ":dark-matter:243"
    "Starfall"
    difficulty
    [ "theTatterdemalion . . . . . lhourglass hourglass rhourglass ldiamond diamond rdiamond . . theCassilda"
    , "lplus plus rplus ldroplet droplet rdroplet lcircle circle rcircle lheart heart rheart lmoon moon rmoon"
    , ". . . lequals equals requals lsquare square rsquare ltriangle triangle rtriangle ltrefoil trefoil rtrefoil"
    ]

{- | Scenario reference card, ":dark-matter:243" (the front block is
Easy \/ Standard, from @cards\/sf_01_starfall.json@; the Hard \/ Expert block is
user-supplied in @act-backs.md@):

Easy \/ Standard
[skull]: -X. X is half the number of revealed locations without clues (rounded up).
[cultist]: -3. If you fail by 2 or more, take 1 damage and 1 horror.
[tablet]: Increase the difficulty of this skill test by 1. Reveal another token.
[elder thing]: -3. If you fail, you must either place 1 doom on the current agenda, or take 1 horror for each point you fail by.

Hard \/ Expert
[skull]: -X. X is half the number of locations in play (rounded up).
[cultist]: -4. If you fail by 2 or more, take 1 damage and 1 horror.
[tablet]: Increase the difficulty of this skill test by 1. Reveal another token.
[elder thing]: -4. If you fail, you must either place 1 doom on the current agenda, or take 1 horror for each point you fail by.

Only the /values/ live here; the riders are the 'ResolveChaosToken' and
'FailedSkillTest' cases in 'RunMessage' below. [skull] counts a different pool
per difficulty — revealed locations holding no clues on easy\/standard, every
location in play on hard\/expert — and [tablet] has no value at all.
-}
instance HasChaosTokenValue Starfall where
  getChaosTokenValue iid tokenFace (Starfall attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ byDifficulty attrs (RevealedLocation <> LocationWithoutClues) Anywhere
      pure $ ChaosTokenValue Skull (NegativeModifier $ (n + 1) `div` 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage Starfall where
  runMessage msg s@(Starfall attrs) = runQueueT $ scenarioI18n "starfall" $ case msg of
    PreScenarioSetup -> do
      {- "Check your Campaign Log. If the investigators know of the Abjuration of
      the Throne, proceed to Another Path. Otherwise, skip to Setup." Another Path
      is flavor only; the record itself is read by the Ritual of the Sun story. -}
      anotherPath <- getHasRecord TheInvestigatorsKnowOfTheAbjurationOfTheThrone
      flavor $ scope "intro" do
        h "title"
        p "body"
        ul do
          li.validate anotherPath "proceedToAnotherPath"
          li.validate (not anotherPath) "skipToSetup"
      when anotherPath $ flavor $ scope "anotherPath" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup Starfall attrs do
      stabilized <- getHasRecord TheElbrusStationHasBeenFullyStabilized
      nostalgiaSaved <- getHasRecord TheNostalgiaIIHasBeenSaved
      alliedWithMiGo <- getHasRecord YouAreAlliedWithTheMiGo

      setup $ ul do
        li "gatherSets"
        li "setAsideHastursGaze"
        li "setAsideCards"
        li "setAsideAgenda"
        li.nested "checkCampaignLog" do
          li "elbrusStation"
          li "nostalgiaII"
          li "miGoAlliance"
        li "createScanningDeck"
        li.nested "placeLocations" do
          li "startAt"
        li "impendingDoom"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all cards from the following encounter sets: Starfall,
      Anachronism, Deep Space, Endtimes, and Agents of Yog-Sothoth." -}
      gather Set.Starfall
      gather Set.Anachronism
      gather Set.DeepSpace
      gather Set.Endtimes
      gather Set.AgentsOfYogSothoth

      -- "Set aside the Hastur's Gaze encounter set." Red Sun shuffles the whole
      -- set into the encounter deck when it advances.
      gatherAndSetAside Set.HastursGaze

      {- "For every tally mark under 'Impending Doom' in your Campaign Log, begin
      the scenario with 1 additional doom on the agenda." Preloaded so Journey
      Across Space enters play already holding it, rather than having doom placed
      on it behind its own EnterPlay window. -}
      impendingDoom <- getImpendingDoom
      when (impendingDoom > 0)
        $ scenarioSetupModifier
          attrs.id
          attrs
          (AgendaId $ toCardCode Agendas.journeyAcrossSpace)
          (EntersPlayWithDoom impendingDoom)

      setAgendaDeck [Agendas.journeyAcrossSpace, Agendas.redSun, Agendas.supernova]
      setActDeck [Acts.endTimes]

      {- "Set the following cards aside, out of play: the The Cassilda location,
      the Domaag T'eel enemy, the Mi-Go Sentinel enemy, the Yithian Guard enemy,
      and The Feaster from Afar." -}
      setAside
        [ Locations.theCassilda
        , Enemies.domaagTeel
        , Enemies.miGoSentinel
        , Enemies.yithianGuard
        , Enemies.theFeasterFromAfar
        ]

      -- "Set aside the Dark Matter agenda and Tassilda's Awakening act, as well
      -- as the Tassilda enemy, out of play." Ritual of the Sun swaps them in.
      setAside [Agendas.darkMatter, Acts.tassildasAwakening, Enemies.tassilda]

      {- "Check your Campaign Log: If Elbrus Station has been fully stabilized,
      set aside the Ar-NO story asset, out of play. Otherwise, set aside the
      Project Origami story asset [...]" — all six of these have scanning backs,
      so the three that are not set aside stay in the pool and end up in the
      scanning deck below. -}
      setAside
        [ if stabilized then Assets.arNO else Assets.projectOrigami
        , if nostalgiaSaved then Assets.directorCixin else Assets.lastHope
        , if alliedWithMiGo then Assets.miGoCollector else Assets.repairingTheThreshold
        ]

      addScanningDeck

      {- "Put the following locations into play: The Tatterdemalion, Mars,
      Asteroid Belt and Pluto. Each investigator begins play at The
      Tatterdemalion." Mars, Asteroid Belt and Pluto..? are the unrevealed faces
      of Hope, New Brooklyn and Yuggoth. -}
      placeAll [Locations.hope, Locations.newBrooklyn, Locations.yuggoth]
      startAt =<< place Locations.theTatterdemalion
    {- [tablet]: "Increase the difficulty of this skill test by 1. Reveal another
    token." The increase is queued first so it lands before the replacement token
    is revealed. -}
    ResolveChaosToken _ Tablet iid -> do
      push $ IncreaseSkillTestDifficulty 1
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        -- [cultist]: "If you fail by 2 or more, take 1 damage and 1 horror."
        Cultist -> when (n >= 2) $ assignDamageAndHorror iid Cultist 1 1
        {- [elder thing]: "If you fail, you must either place 1 doom on the
        current agenda, or take 1 horror for each point you fail by." Mandatory,
        so both options are always offered. -}
        ElderThing -> chooseOneM iid $ unscoped do
          countVar 1 $ labeled "placeDoomOnAgenda" $ placeDoomOnAgenda 1
          countVar n $ labeled "takeHorror" $ assignHorror iid ElderThing n
        _ -> pure ()
      pure s
    Resign iid -> do
      inHope <- iid <=~> InvestigatorAt (locationIs Locations.hope)
      inNewBrooklyn <- iid <=~> InvestigatorAt (locationIs Locations.newBrooklyn)
      let meta = toResultDefault emptyMeta attrs.meta
      pure
        $ Starfall
        $ attrs
        & metaL
        .~ toJSON
          ( meta
              { resignedInHope = nub $ [iid | inHope] <> meta.resignedInHope
              , resignedInNewBrooklyn = nub $ [iid | inNewBrooklyn] <> meta.resignedInNewBrooklyn
              }
          )
    ScenarioResolution r -> do
      case r of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): Proceed to Resolution 1." -}
        NoResolution -> do
          scope "resolutions" $ resolution "noResolution"
          push R1
        Resolution 1 -> resolution1 attrs
        Resolution 2 -> resolution2 attrs
        {- The Dark Matter agenda's back ("Each investigator is killed. The
        investigators lose the campaign. There is no resolution.") deliberately
        never reaches here: it claims the scenario's no-remaining-investigators
        handler so that killing the last investigator does not fall through to
        NoResolution — and hence to Resolution 1. -}
        _ -> error "invalid resolution"
      pure s
    _ -> Starfall <$> liftRunMessage msg attrs

{- | "A brilliant light shines over the solar system..."

Each story asset in the victory display writes its record; the investigators who
resigned at a colony that survived get its ending, and everyone left over reads
The End 1 (nothing was saved, campaign lost) or The End 2.
-}
resolution1 :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m ()
resolution1 attrs = do
  miGoReturned <- isInVictoryDisplay Assets.repairingTheThreshold
  hopeShielded <- isInVictoryDisplay Assets.lastHope
  uccEscaped <- isInVictoryDisplay Assets.projectOrigami
  scope "resolutions" $ resolution "resolution1"
  record TheSunWentSupernovaAndWipedOutMostOfTheSolarSystem
  recordWhen miGoReturned MiGoSafelyReturnedToTheirHomeWorld
  recordWhen hopeShielded HopeWasShieldedFromTheBlast
  recordWhen uccEscaped TheUCCEscapedToAnotherGalaxy
  let meta = toResultDefault emptyMeta attrs.meta
  let withHope = if hopeShielded then meta.resignedInHope else []
  let onParadise = if uccEscaped then meta.resignedInNewBrooklyn else []
  survivors "beyondHope" LivedTheRemainderOfTheirDaysWithHope withHope
  survivors "farFarAway" LivedTheRemainderOfTheirDaysOnParadise onParadise
  remaining <- filter (`notElem` (withHope <> onParadise)) <$> allInvestigators
  if miGoReturned || hopeShielded || uccEscaped
    then do
      resolutionOnly remaining $ scope "theEnd2" $ setTitle "title" >> p "body"
      for_ remaining \iid -> do
        recordForInvestigator iid WasCaughtInTheSupernovasBlast
        kill attrs iid
      endOfScenario
    else do
      resolutionOnly remaining $ scope "theEnd1" $ setTitle "title" >> p "body"
      record YouHaveReturnedToThePalacesOfDimCarcosa
      for_ remaining (kill attrs)
      -- "The investigators lose the campaign." — The End 1 has no epilogue.
      gameOver
 where
  survivors key logKey iids = do
    resolutionOnly iids $ scope key $ setTitle "title" >> p "body"
    for_ iids (`recordForInvestigator` logKey)

-- | "The Royal Princess of Carcosa succumbs to your assault..." -> The End 3.
resolution2 :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m ()
resolution2 attrs = do
  scope "resolutions" $ resolution "resolution2"
  earnXp attrs "theEnd3"
  record TheInvestigatorsEscapedHastursGrasp
  eachInvestigator \iid -> do
    mental <- field InvestigatorMentalTrauma iid
    when (mental > 0) $ chooseOneM iid do
      for_ (reverse [1 .. min 2 mental]) \n ->
        unscoped $ countVar n $ labeled "healMentalTrauma" $ push $ HealTrauma iid 0 n
      unscoped $ labeled "doNotHeal" nothing
  record HopeWasShieldedFromTheBlast
  record TheUCCEscapedToAnotherGalaxy
  record MiGoSafelyReturnedToTheirHomeWorld
  endOfScenario
