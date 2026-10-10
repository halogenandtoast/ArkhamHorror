{- | Scenario V. An open-ended year of globe-trotting preparation.

There is one act, /The Long Game/, and it never advances on progress: the year
ends either when every undefeated investigator has resigned or when Summer runs
out. Both roads lead to the same place, so Resolution 1 is the only resolution
and 'NoResolution' is routed into it.

What the investigators actually /do/ is the __Task__ deck: eleven Task
treacheries from the @missions@ set, six shuffled into the 'TaskDeck' and five
arriving on the agendas' schedule. Completing a Task removes it from the game,
and "which Tasks you've completed" is read back out of the removed-from-play
pile -- see "Arkham.Homebrew.AgesUnwound.Missions.Helpers", which owns that
convention. Every Task left incomplete at the end records a campaign-log key
against the investigators, and Scenario VI is harder for each one.
-}
module Arkham.Homebrew.AgesUnwound.Scenarios.AYearToPlan (aYearToPlan) where

import Arkham.Act.Types (Field (ActResources))
import Arkham.Card
import Arkham.Helpers.FlavorText (flavor, h, li, p, setup, ul)
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getLead, getSetAsideCardsMatching)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Skills qualified as Skills
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (scenarioI18n)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (
  getCompletedTaskCount,
  taskCompleted,
  taskDeckCards,
 )
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern TaskDeck)
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, recordForInvestigator)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Token qualified as Token
import Arkham.Treachery.CardDefs.NightOfTheZealot.TheMidnightMasks qualified as MidnightMasks

newtype AYearToPlan = AYearToPlan ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The year's travel map. All 15 printed connections join grid-adjacent cells
with no crossings: Arkham and the Americas sit along the bottom row, Europe and
the Mediterranean in the middle, and the far-flung sites (Tunguska, the
Himalayas, Shanghai) across the top. Each location carries 'symbolLabel', so its
cell is its symbol; the two @missions@ locations (Another Realm, The British
Library) are included because the scenario can put them into play.
-}
aYearToPlan :: Difficulty -> AYearToPlan
aYearToPlan difficulty =
  scenario
    AYearToPlan
    ":ages-unwound:105"
    "A Year to Plan"
    difficulty
    [ ".         star  moon   equals   ."
    , "hourglass heart t      squiggle triangle"
    , "droplet   plus  square circle   diamond"
    ]

{- | "Put each City and Wilderness location into play." The eleven besides Arkham,
Massachusetts, which setup places on its own so it can hold the id the
investigators start at.

No grid: the board is a travel map stitched together by the locations' own
connection symbols plus the printed @[action][action]: __Move__@ abilities.
Sydney prints no symbol and no connections at all, so the two move abilities
that name it are the only way in.
-}
cityAndWildernessLocations :: [CardDef]
cityAndWildernessLocations =
  [ Locations.sanFrancisco
  , Locations.mexicoCity
  , Locations.london
  , Locations.paris
  , Locations.rome
  , Locations.istanbul
  , Locations.cairo
  , Locations.tunguska
  , Locations.himalayas
  , Locations.shanghai
  , Locations.sydney
  ]

{- | Resolution 1's rewards: "For each of the following completed Tasks, any one
investigator may choose to add the corresponding player card to their deck."
-}
taskRewards :: [(CardDef, CardDef)]
taskRewards =
  [ (Treacheries.enemyOfMyEnemy, Skills.monasticTraining)
  , (Treacheries.aTreasureUnearthed, Assets.chronalAtlas)
  , (Treacheries.theDevilYouKnow, Assets.ionianPendant)
  , (Treacheries.higherPowers, Events.agencyStrikeTeam)
  , (Treacheries.entreatingTheGods, Assets.wingsOfDamakairon)
  , (Treacheries.upToSomething, Assets.forestallFate)
  ]

{- | Resolution 1: "For each copy of A Multitude of Plots in the victory display,
record one of the following statements of your choice in your Campaign Log, as
you fail to stop the Myriad from enacting one of their plans."
-}
myriadPlans :: [(Text, AgesUnwoundKey)]
myriadPlans =
  [ ("raisedAPowerfulWarding", TheMyriadRaisedAPowerfulWarding)
  , ("recruitedACruelSorcerer", TheMyriadRecruitedACruelSorcerer)
  , ("weavedADreadCurse", TheMyriadWeavedADreadCurse)
  ]

{- | Scenario reference card, @:ages-unwound:105@:

Easy / Standard
[skull]: -X. X is half the number of completed [[Tasks]] (rounded down).
[cultist]: -5. Draw a card or gain a resource.
[tablet]: -2. If you fail and it is your turn, lose all remaining actions and end your turn.
[elder_thing]: -3. If you fail, take 1 damage and 1 horror.

Hard / Expert
[skull]: -X. X is the number of completed [[Tasks]].
[cultist]: -8. Draw a card or gain a resource.
[tablet]: -4. If you fail and it is your turn, lose all remaining actions and end your turn.
[elder_thing]: -5. If you fail, take 1 damage and 1 horror.

The [skull] rewards the whole table for the Tasks it has banked, which is the
scenario's own scoreboard. There is always an [elder_thing] in the bag here:
Scenario III's resolutions put one in and nothing before Resolution 1 takes it
out again.
-}
instance HasChaosTokenValue AYearToPlan where
  getChaosTokenValue iid tokenFace (AYearToPlan attrs) = case tokenFace of
    Skull -> do
      n <- getCompletedTaskCount
      pure $ ChaosTokenValue Skull $ NegativeModifier $ byDifficulty attrs (n `div` 2) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 5 8
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage AYearToPlan where
  runMessage msg s@(AYearToPlan attrs) = runQueueT $ scenarioI18n "aYearToPlan" $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup AYearToPlan attrs do
      setup $ ul do
        li "gatherSets"
        li "midnightMasks"
        li.nested "placeLocations" do
          li "startAt"
        li "helpingYourself"
        li "taskDeck"
        li "setAside"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      {- "Gather all cards from the following encounter sets: A Year to Plan,
      Missions, Myriad, Shifting Reality, Unravelling Years, Dark Cult and The
      Midnight Masks." -}
      gather Set.AYearToPlan
      gather Set.Missions
      gather Set.Myriad
      gather Set.ShiftingReality
      gather Set.UnravellingYears
      gather Set.DarkCult

      {- "When gathering The Midnight Masks encounter set, only gather the 5
      treachery cards (2x False Lead and 3x Hunting Shadow). Do not gather the
      location, act, agenda, or scenario reference cards from that set."

      The official cards, reused -- this campaign adds nothing of its own here. -}
      gatherJust Set.TheMidnightMasks [MidnightMasks.falseLead, MidnightMasks.huntingShadow]

      setActDeck [Acts.theLongGame]
      setAgendaDeck [Agendas.autumn, Agendas.winter, Agendas.spring, Agendas.summer]

      {- "Each investigator begins play in Arkham, Massachusetts." Placed on its
      own because 'placeAll' hands back nothing and the locations do not exist
      in game state until its queued PlaceLocation messages run -- a select here
      would find no board at all. -}
      arkham <- place Locations.arkhamMassachusetts_111
      placeAll cityAndWildernessLocations
      startAt arkham

      {- "Shuffle the following treachery cards together to form the task deck:
      Enemy of my Enemy, A Treasure Unearthed, The Devil You Know, Higher
      Powers, Entreating the Gods and Up To Something. Place this deck near the
      scenario reference card."

      Built before the @missions@ remainder is set aside, so the six are pulled
      out of the gathered cards rather than minted fresh. The deck's physical
      spot next to the scenario reference card has no engine representation --
      a 'ScenarioDeckKey' deck is not a placed card -- so it renders wherever
      the client draws scenario decks. -}
      addExtraDeck TaskDeck =<< shuffleM taskDeckCards

      {- "Set the remainder of the Missions encounter set aside, out of play,
      along with each copy of Curse of a Thousand Winters."

      Every other Task, the locations and enemies the Tasks put into play, and
      the six player-card rewards all live in the set-aside pool, which is where
      'Arkham.Helpers.FetchCard' looks first. -}
      setAsideEvery $ CardFromEncounterSet Set.Missions
      setAsideEvery $ cardIs Treacheries.curseOfAThousandWinters

      {- "Put the Helping Yourself treachery into play next to the act deck with
      X resources on it. X is the number of marks under Strange Assistance in
      your Campaign Log."

      'Arkham.Homebrew.AgesUnwound.Missions.Helpers.putTaskIntoPlay' is the Task
      convention for this placement but hands back no id, and the resources are
      the scenario's to seed -- so the same creation is made here with the id
      captured. Helping Yourself prints no __Revelation__, so nothing is
      resolved; Arkham, Massachusetts is the only thing that takes a resource
      back off, and removing the last one completes the Task. -}
      strangeAssistance <- getRecordCount StrangeAssistance
      helpingYourself <- fetchCard Treacheries.helpingYourself
      (helpingYourselfId, createHelpingYourself) <-
        Msg.createTreacheryAt helpingYourself NextToAct
      push createHelpingYourself
      placeTokens attrs helpingYourselfId Token.Resource strangeAssistance
    -- [cultist]: "Draw a card or gain a resource."
    ResolveChaosToken _ Cultist iid -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "drawCards" $ drawCards iid Cultist 1
        countVar 1 $ labeled "gainResources" $ gainResources iid Cultist 1
      pure s
    {- [tablet]: "If you fail and it is your turn, lose all remaining actions and
    end your turn."

    "All remaining actions" is 'SetActions' to 0 rather than a
    'LoseStandardActions' count: it also marks every additional action used,
    which is what "all" means. -}
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Tablet -> do
      isTurn <- iid <=~> TurnInvestigator
      when isTurn do
        setActions iid Tablet 0
        endYourTurn iid
      pure s
    -- [elder thing]: "If you fail, take 1 damage and 1 horror."
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == ElderThing -> do
      assignDamage iid ElderThing 1
      assignHorror iid ElderThing 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): Read Resolution 1." Summer's back and The Long Game's
        objective both also point at R1, so it is the only ending. -}
        NoResolution -> push R1
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheRitualIsNigh

          {- "For each investigator that did not resign, record in your Campaign
          Log that [investigator name] returned to Arkham late."

          Defeated investigators did not resign either, so the select has to
          reach past elimination -- 'not_ ResignedInvestigator' on its own
          excludes them. Scenario VI reads this back with
          'InvestigatorWithRecord', which is why it is a per-investigator record
          rather than a card-code record set. -}
          late <- select $ IncludeEliminated $ not_ ResignedInvestigator
          for_ late (`recordForInvestigator` ReturnedToArkhamLate)

          {- The four Tasks whose failure advances the Myriad. Each condition is
          kept as a local so the chaos-bag block below can reuse it: a
          'getHasRecord' there would read the log /before/ the queued 'record'
          messages had been applied. -}
          violatedCausality <- not <$> taskCompleted Treacheries.helpingYourself
          when violatedCausality $ record TheInvestigatorsViolatedCausality

          {- "If Keeper of Knowledge is not completed and Ancient Sphinx is in
          play." The sphinx clause is the guide's own guard for a year that
          never reached Autumn's back, where that Task enters play. -}
          keeperIncomplete <- not <$> taskCompleted Treacheries.keeperOfKnowledge
          sphinxInPlay <- selectAny $ enemyIs Enemies.ancientSphinx
          let solvedTheRiddle = keeperIncomplete && sphinxInPlay
          when solvedTheRiddle $ record TheMyriadSolvedTheRiddleOfTheSphinx

          unlessM (taskCompleted Treacheries.strangePortal)
            $ record TheMyriadHarnessedThePowerOfAnotherRealm
          unlessM (taskCompleted Treacheries.theTunguskaEvent)
            $ record TheMyriadTookControlOfAColourOutOfSpace

          {- "For each of the following completed Tasks, any one investigator may
          choose to add the corresponding player card to their deck. Each of
          these cards does not count towards their owner's deck size." -}
          investigators <- select InvestigatorCanAddCardsToDeck
          for_ taskRewards \(task, reward) ->
            whenM (taskCompleted task)
              $ addCampaignCardToDeckChoice investigators DoNotShuffleIn reward

          {- "For each copy of A Multitude of Plots in the victory display, record
          one of the following statements of your choice." One choice per copy,
          so the loop has to be a 'doStep' countdown with its own 'doNextStep'. -}
          plots <-
            count (`cardMatch` cardIs Treacheries.aMultitudeOfPlots) <$> getVictoryDisplay

          {- The chaos-bag block from the structure notes, not the guide's OCR
          (its token glyphs are unreliable): remove all [cultist], [tablet] and
          [elder_thing]; add 1 [cultist] and 1 [tablet]; add 1 [cultist] per
          resource on the current act; +2 [tablet] if the Myriad solved the
          riddle of the sphinx; +2 [elder_thing] if the investigators violated
          causality. Every add is campaign-scoped, so the bag carries into
          Scenarios VI and VII. -}
          for_ [Cultist, Tablet, ElderThing] removeAllChaosTokens
          addChaosToken Cultist
          addChaosToken Tablet

          actResources <- selectOne AnyAct >>= maybe (pure 0) (field ActResources)
          replicateM_ actResources $ addChaosToken Cultist
          when solvedTheRiddle $ replicateM_ 2 $ addChaosToken Tablet
          when violatedCausality $ replicateM_ 2 $ addChaosToken ElderThing

          {- The dread curse is the players' choice above, so it can only be read
          once the countdown has run out -- hence the @DoStep 0@ tail below
          rather than a check here. -}
          doStep plots msg
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    DoStep n (ScenarioResolution (Resolution 1)) | n > 0 -> scope "resolutions" do
      lead <- getLead
      chooseOneM lead $ for_ myriadPlans \(key, k) -> labeled key $ record k
      doNextStep msg
      pure s
    {- "If the Myriad weaved a dread curse, each investigator must add 1 copy of
    Curse of a Thousand Winters to their deck. This card does not count towards
    deck size." -}
    DoStep 0 (ScenarioResolution (Resolution 1)) -> do
      whenM (getHasRecord TheMyriadWeavedADreadCurse) do
        {- One distinct set-aside copy per investigator.
        'AddCampaignCardToDeck' re-owns the card it is handed and does not
        consume the set-aside pool, so passing the def would return the same
        card to everyone and they would all share one card id. Four printed
        copies, at most four investigators: the pairing is exact. -}
        curses <- getSetAsideCardsMatching $ cardIs Treacheries.curseOfAThousandWinters
        investigators <- select InvestigatorCanAddCardsToDeck
        for_ (zip investigators curses) \(iid, card) ->
          addCampaignCardToDeck iid DoNotShuffleIn card
      pure s
    _ -> AYearToPlan <$> liftRunMessage msg attrs
