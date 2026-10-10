{- | Scenario II. Baxter's manor; the introduction branches on Interlude I's
choice (guide p5-6).

Besides setup and the two resolutions, this module owns the two halves of the
scenario's "Copies of Enemies" mechanic that have to live somewhere central --
see "Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers" for why
the copies are ordinary enemies rather than swarm cards:

* @ScenarioSpecific@ registration, so two spawns in one handler cannot clobber
  each other the way a @SetScenarioMeta@ read-modify-write would.
* @RemoveEnemy@ / @Discarded@: return the card to the bottom of its owner's deck
  and keep the stand-in encounter card out of the encounter discard pile.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman (theMyriadGentleman) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits (pattern Myriad)
import Arkham.Id (EnemyId)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheMyriadGentleman = TheMyriadGentleman ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Baxter's manor. Every one of the 13 printed connections joins grid-adjacent
cells and no two connection lines cross, so the garden (circle/diamond/square/
triangle) and the house interior stay visually separate as the manor opens up.
Each location carries 'symbolLabel', so its cell is its symbol.
-}
theMyriadGentleman :: Difficulty -> TheMyriadGentleman
theMyriadGentleman difficulty =
  scenario
    TheMyriadGentleman
    ":ages-unwound:023"
    "The Myriad Gentleman"
    difficulty
    [ "hourglass star     .      ."
    , "heart     squiggle circle triangle"
    , "moon      equals   diamond square"
    ]

{- | "Set each remaining location aside, out of play" -- every Manor location.
The four Garden locations are the ones that start on the table.
-}
setAsideLocations :: [CardDef]
setAsideLocations =
  [ Locations.entranceHall
  , Locations.parlor
  , Locations.kitchen
  , Locations.landing
  , Locations.masterBedroom
  , Locations.study
  ]

instance HasChaosTokenValue TheMyriadGentleman where
  getChaosTokenValue iid tokenFace (TheMyriadGentleman attrs) = case tokenFace of
    -- "-1 (-3 instead if there is a [[Myriad]] enemy at your location)" /
    -- "-2 (-4 instead)".
    Skull -> do
      atYour <- selectAny $ EnemyWithTrait Myriad <> enemyAtLocationWith iid
      pure
        $ toChaosTokenValue attrs Skull (if atYour then 3 else 1) (if atYour then 4 else 2)
    -- "-1. If you succeed, deal 1 damage to a [[Myriad]] enemy at your location."
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    {- "-2. If you fail and it is Act 2 or 3, spawn a copy of The Myriad Gentleman
    engaged with you." Hard/Expert is "-3. After this test ends, if it is Act 2 or
    3, ..." -- no longer conditional on failing. -}
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMyriadGentleman where
  runMessage msg s@(TheMyriadGentleman attrs) = runQueueT $ scenarioI18n $ case msg of
    {- "Check Campaign Log. If the investigators accepted an offer of help: Proceed
    to Intro 1. If the investigators declined an offer of help: Skip to Intro 2."
    Both continue to Intro 3. -}
    PreScenarioSetup -> scope "intro" do
      accepted <- getHasRecord TheInvestigatorsAcceptedAnOfferOfHelp
      flavor $ setTitle "title" >> p (if accepted then "intro1" else "intro2")
      flavor $ setTitle "title" >> p "intro3"
      pure s
    Setup -> runScenarioSetup TheMyriadGentleman attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setAsideLocations"
        li "setAsideCards"
        li "copiesOfEnemies"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all cards from the following encounter sets: The Myriad Gentleman,
      Shifting Reality, Unravelling Years, Agents of Yog-Sothoth, Locked Doors."
      The guide's "Unravelling Ages" is the data's Unravelling Years. -}
      gather Set.TheMyriadGentleman
      gather Set.ShiftingReality
      gather Set.UnravellingYears
      gather Set.AgentsOfYogSothoth
      gather Set.LockedDoors

      setAgendaDeck [Agendas.ripplesInReality, Agendas.armyOfOne, Agendas.fromOneMany]
      setActDeck [Acts.aFineCountryGarden, Acts.justOneMan, Acts.swarmed]

      -- "Put the Lawn, Hedge Maze, Ornate Fountain and Stables locations into play.
      -- Each investigator begins play at the Lawn."
      placeAll [Locations.hedgeMaze, Locations.ornateFountain, Locations.stables]
      startAt =<< place Locations.lawn

      {- "Set each remaining location aside, out of play, along with each copy of
      The Myriad Gentleman, Ex Uno Plures, They Just Keep Coming, Not Welcome
      Here, Aforgomon's Blade and Gaze of Aforgomon." Fragmented Existence is
      deliberately NOT set aside -- it stays in the encounter deck. -}
      setAside setAsideLocations
      setAside [Enemies.theMyriadGentleman_042, Enemies.theMyriadGentleman_043]
      setAsideEvery
        $ mapOneOf
          cardIs
          [Treacheries.exUnoPlures, Treacheries.theyJustKeepComing, Treacheries.notWelcomeHere]
      setAside [Assets.aforgomonsBlade]
      setAside [Treacheries.gazeOfAforgomon]

      setMeta emptyMyriadMeta

      -- Intro 2: "When the game begins, place 1 doom on the current agenda."
      unlessM (getHasRecord TheInvestigatorsAcceptedAnOfferOfHelp) $ placeDoomOnAgenda 1
    -- [cultist]: "If you succeed, deal 1 damage to a [[Myriad]] enemy at your location."
    Msg.PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      myriad <- select $ EnemyWithTrait Myriad <> enemyAtLocationWith iid
      chooseTargetM iid myriad $ nonAttackEnemyDamage (Just iid) Cultist 1
      pure s
    -- [tablet], easy/standard: "If you fail and it is Act 2 or 3, spawn a copy of
    -- The Myriad Gentleman engaged with you."
    Msg.FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Tablet
      , isEasyStandard attrs -> do
          whenM onActTwoOrThree $ spawnMyriadCopiesEngagedWith iid 1
          pure s
    -- [tablet], hard/expert: "After this test ends, if it is Act 2 or 3, spawn a
    -- copy of The Myriad Gentleman engaged with you."
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      withSkillTest \sid -> afterThisTestResolves sid do
        whenM onActTwoOrThree $ spawnMyriadCopiesEngagedWith iid 1
      pure s
    -- "Copies of Enemies": the scenario is the one place that remembers which
    -- player card each copy was made from.
    ScenarioSpecific key v | key == registerMyriadCopyKey -> do
      let copy = toResult @MyriadCopy v
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      pure $ TheMyriadGentleman $ attrs & metaL .~ toJSON (MyriadMeta $ copy : meta.copies)
    {- "When that enemy leaves play, the card's owner places it on the bottom of
    their deck." 'RemoveEnemy' is the single point every leave-play path funnels
    through (defeat, discard, removal), and the scenario runs before
    'Arkham.Game.Runner' handles it. -}
    Msg.RemoveEnemy eid -> do
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      for_ (find (\c -> c.enemy == eid && not c.returned) meta.copies) \copy -> do
        scenarioSpecific returnedMyriadCopyKey eid
        push
          $ Msg.PutCardOnBottomOfDeck copy.owner (Deck.InvestigatorDeck copy.owner) copy.card
      TheMyriadGentleman <$> liftRunMessage msg attrs
    ScenarioSpecific key v | key == returnedMyriadCopyKey -> do
      let eid = toResult @EnemyId v
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      let mark c = if c.enemy == eid then c {myriadCopyReturned = True} else c
      pure $ TheMyriadGentleman $ attrs & metaL .~ toJSON (MyriadMeta $ map mark meta.copies)
    {- A copy's card is an encounter card standing in for a player card, so the
    default bookkeeping would file it in the encounter discard pile and it could
    be drawn as an encounter card. Swallow the message; 'RemoveEnemy' above has
    already sent the real card home. -}
    Msg.Discarded (EnemyTarget eid) _ _ -> do
      let meta = toResultDefault emptyMyriadMeta attrs.meta
      if any ((== eid) . (.enemy)) meta.copies
        then pure s
        else TheMyriadGentleman <$> liftRunMessage msg attrs
    ScenarioResolution res -> scope "resolutions" do
      case res of
        -- "If no resolution was reached (each investigator was defeated): Read
        -- Resolution 2."
        NoResolution -> push R2
        Resolution 1 -> do
          record TheInvestigatorsLearntOfTheMyriadsRitual
          offerAforgomonsBlade
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsFledTheGentlemansManor
          offerAforgomonsBlade
          resolutionWithXp "resolution2" $ allGainXp' attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheMyriadGentleman <$> liftRunMessage msg attrs

{- | "If the Myriad Gentleman (Master of the House) is in the victory display, any
one investigator may choose to add Aforgomon's Blade to their deck. If an
investigator chooses to include Aforgomon's Blade in their deck, they must also
add the Gaze of Aforgomon weakness to their deck. These cards do not count
towards that investigator's deck size." -- which is 'DoNotShuffleIn' plus the
blade's continuation adding the weakness to the same investigator.
-}
offerAforgomonsBlade :: ReverseQueue m => m ()
offerAforgomonsBlade = do
  defeated <-
    selectAny
      $ VictoryDisplayCardMatch
      $ basic (cardIs Enemies.theMyriadGentleman_043)
  when defeated do
    iids <- allInvestigators
    gaze <- fetchCard Treacheries.gazeOfAforgomon
    addCampaignCardToDeckChoiceWith iids DoNotShuffleIn Assets.aforgomonsBlade \iid ->
      [Msg.AddCampaignCardToDeck iid DoNotShuffleIn gaze]
