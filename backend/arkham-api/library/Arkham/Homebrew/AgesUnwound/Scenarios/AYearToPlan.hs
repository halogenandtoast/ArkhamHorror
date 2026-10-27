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
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.FlavorText (
  FlavorTextBuilder,
  flavor,
  h,
  li,
  p,
  resolutionFlavor,
  setTitle,
  setup,
  ul,
 )
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Events
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Skills qualified as Skills
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.DummyDeck (setUpDummyDeck)
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

{- | The year's travel map. Columns run west to east by longitude -- San
Francisco and Mexico City, then Arkham and London, then Paris/Rome/Cairo,
Istanbul and the Himalayas, and the Pacific rim -- and none of the 15 printed
connections cross or passes through a third card.

Three rows cannot also encode 95 degrees of latitude, and longitude is the axis
worth keeping: east-to-west is what makes the board read as a map. So Arkham
sits directly above London rather than an ocean away. That placement is forced
rather than chosen -- the Americas need two columns to draw SF/Mexico
City/Arkham as a triangle at all (in one column San Francisco falls between the
other two and the Arkham-Mexico City line runs through it), and the eastern
chain needs three -- and it reads well anyway, because Arkham's printed
@[action][action]: __Move__@ destinations are London and Paris and it is
orthogonally adjacent to both. The Atlantic is an ability here, not a line.

/Another Realm/ is an Otherworld with no place on Earth. It attaches only to
Paris, which prints @Square@, so it takes the one free cell -- immediately right
of Paris.

Every row and column holds at least one card on purpose. The client's grid
sizes its tracks @auto@/@max-content@, so a wholly empty column collapses to
zero width and would bend the connection lines through their neighbours.

Each location carries 'symbolLabel', so its cell is its symbol -- except Sydney,
which prints no symbol and so takes the label 'Arkham.Name.nameToLabel' derives
from its title.
-}
aYearToPlan :: Difficulty -> AYearToPlan
aYearToPlan difficulty =
  scenario
    AYearToPlan
    ":ages-unwound:105"
    "A Year to Plan"
    difficulty
    [ "diamond  circle  t        square heart"
    , "triangle plus    squiggle moon   hourglass"
    , ".        droplet equals   star   sydney"
    ]

{- | "Put each City and Wilderness location into play." The eleven besides Arkham,
Massachusetts, which setup places on its own so it can hold the id the
investigators start at.

No grid adjacency: the layout above only positions the cards, and the board is
stitched together by the locations' own connection symbols plus the printed
@[action][action]: __Move__@ abilities.
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

The leading key is the resolution's bullet for that Task, so the panel's ticks
and the deck offer walk the same list in the same order.
-}
taskRewards :: [(String, CardDef, CardDef)]
taskRewards =
  [ ("enemyOfMyEnemy", Treacheries.enemyOfMyEnemy, Skills.monasticTraining)
  , ("aTreasureUnearthed", Treacheries.aTreasureUnearthed, Assets.chronalAtlas)
  , ("theDevilYouKnow", Treacheries.theDevilYouKnow, Assets.ionianPendant)
  , ("higherPowers", Treacheries.higherPowers, Events.agencyStrikeTeam)
  , ("entreatingTheGods", Treacheries.entreatingTheGods, Assets.wingsOfDamakairon)
  , ("upToSomething", Treacheries.upToSomething, Assets.forestallFate)
  ]

{- | Resolution 1: "For each copy of A Multitude of Plots in the victory display,
record one of the following statements of your choice in your Campaign Log, as
you fail to stop the Myriad from enacting one of their plans."

The key names both the statement as the resolution prints it and, under
@label@, the button the lead picks it with.
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

      Every other Task, and the locations and enemies the Tasks put into play,
      live in the set-aside pool, which is where 'Arkham.Helpers.FetchCard'
      looks first. -}
      setAsideEvery $ CardFromEncounterSet Set.Missions

      {- The six rewards print the Missions set icon, but they are player cards,
      and a gather cannot supply one: 'gatherEncounterSet' reads
      'allEncounterCards', which holds no homebrew asset, event or skill --
      those register into the player aggregates instead. So the gather above
      never saw them and 'setAsideEvery' pooled nothing, the same silent shape
      'setAsideEveryCopyOf' exists for with weaknesses. Minting also makes each
      a 'PlayerCard', which is what lets /Gratitude/ and /Grudging Assistance/
      set an owner on the copy they hand over. -}
      for_ taskRewards \(_, _, reward) -> setAsideEveryCopyOf reward

      setAsideEveryCopyOf Treacheries.curseOfAThousandWinters

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

      -- Shifting Reality brings Skills of Another Life, which reads "another
      -- investigator's deck". See Arkham.Homebrew.AgesUnwound.DummyDeck.
      setUpDummyDeck
    -- [cultist]: "Draw a card or gain a resource."
    ResolveChaosToken _ Cultist iid -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "drawCards" $ drawCards iid Cultist 1
        countVar 1 $ labeled "gainResources" $ gainResources iid Cultist 1
      pure s
    {- [tablet]: "If you fail and it is your turn, lose all remaining actions and
    end your turn."

    "All remaining actions" is 'SetActions' to 0 rather than a
    'LoseUnrestrictedActions' count: it also marks every additional action used,
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
        NoResolution -> do
          resolution "noResolution"
          push R1
        {- Every record and every chaos-bag change the resolution makes, done
        before a word of it is shown: the panel itself is pushed from the
        'DoStep' tail below, so each of its bullets can be ticked off against
        the log it just wrote. -}
        Resolution 1 -> scope "resolution1" do
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

          actResources <- getActResources
          replicateM_ actResources $ addChaosToken Cultist
          when solvedTheRiddle $ replicateM_ 2 $ addChaosToken Tablet
          when violatedCausality $ replicateM_ 2 $ addChaosToken ElderThing

          void $ allGainXp' attrs

          {- "For each copy of A Multitude of Plots in the victory display, record
          one of the following statements of your choice." One choice per copy,
          so the panel is shown once per copy with that choice attached and once
          more with none left to make. -}
          plots <-
            count (`cardMatch` cardIs Treacheries.aMultitudeOfPlots) <$> getVictoryDisplay
          doStep plots msg
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    -- One pass per copy of A Multitude of Plots, each ending in the lead's choice of plan.
    DoStep n (ScenarioResolution (Resolution 1)) | n > 0 -> scope "resolutions" do
      scope "resolution1" $ resolution1Panel True
      doNextStep msg
      pure s
    {- The last pass: the panel with nothing left to choose, and the two deck
    changes it asks for. Both come after it so the prompts follow the text that
    explains them. -}
    DoStep 0 (ScenarioResolution (Resolution 1)) -> scope "resolutions" do
      scope "resolution1" $ resolution1Panel False
      investigators <- select InvestigatorCanAddCardsToDeck

      {- "For each of the following completed Tasks, any one investigator may
      choose to add the corresponding player card to their deck. Each of these
      cards does not count towards their owner's deck size." -}
      for_ taskRewards \(_, task, reward) ->
        whenM (taskCompleted task)
          $ addCampaignCardToDeckChoice investigators DoNotShuffleIn reward

      {- "If the Myriad weaved a dread curse, each investigator must add 1 copy of
      Curse of a Thousand Winters to their deck. This card does not count towards
      deck size." -}
      whenM (getHasRecord TheMyriadWeavedADreadCurse) do
        {- One distinct set-aside copy per investigator.
        'AddCampaignCardToDeck' re-owns the card it is handed and does not
        consume the set-aside pool, so passing the def would return the same
        card to everyone and they would all share one card id. Four printed
        copies, at most four investigators: the pairing is exact. -}
        curses <- getSetAsideCardsMatching $ cardIs Treacheries.curseOfAThousandWinters
        for_ (zip investigators curses) \(iid, card) ->
          addCampaignCardToDeck iid DoNotShuffleIn card
      pure s
    _ -> AYearToPlan <$> liftRunMessage msg attrs

{- | "For each resource on the current act, add 1 [cultist] token to the chaos
bag." The Long Game is the only act and nothing removes it before the scenario
ends, so the 'Nothing' arm is only reachable from a replay that lost the deck.
-}
getActResources :: HasGame m => m Int
getActResources = selectOne AnyAct >>= maybe (pure 0) (field ActResources)

{- | Resolution 1's instruction list.

Pushed once for each copy of /A Multitude of Plots/ in the victory display, with
the lead's choice of plan attached, and once more when the last copy has been
spent. Every tick is read back out of the campaign log rather than carried
along, so a plan recorded on one pass is already marked on the next and the
final panel shows what the Myriad actually got away with -- which is the only
way the __dread curse__ bullet, whose condition the players set themselves, can
be ticked at all.
-}
resolution1Panel :: (HasI18n, ReverseQueue m) => Bool -> m ()
resolution1Panel withPlotChoice = do
  violatedCausality <- getHasRecord TheInvestigatorsViolatedCausality
  solvedTheRiddle <- getHasRecord TheMyriadSolvedTheRiddleOfTheSphinx
  harnessedAnotherRealm <- getHasRecord TheMyriadHarnessedThePowerOfAnotherRealm
  tookAColour <- getHasRecord TheMyriadTookControlOfAColourOutOfSpace
  weavedADreadCurse <- getHasRecord TheMyriadWeavedADreadCurse
  actResources <- getActResources
  rewards <- for taskRewards \(key, task, _) -> (key,) <$> taskCompleted task

  let
    body :: HasI18n => FlavorTextBuilder ()
    body = do
      setTitle "title"
      p "body"
      ul do
        li "ritualIsNigh"
        li "returnedToArkhamLate"
        li.nested.validate violatedCausality "helpingYourselfIncomplete"
          $ li "violatedCausality"
        li.nested.validate solvedTheRiddle "keeperOfKnowledgeIncomplete"
          $ li "solvedTheRiddle"
        li.nested.validate harnessedAnotherRealm "strangePortalIncomplete"
          $ li "harnessedAnotherRealm"
        li.nested.validate tookAColour "theTunguskaEventIncomplete"
          $ li "tookAColour"
        li.nested "multitudeOfPlots" $ for_ myriadPlans (li . fst)
        li.nested "taskRewards" $ for_ rewards \(key, completed) -> li.validate completed key
        li "chaosTokens"
        li.validate (actResources > 0) "actResourceTokens"
        li.validate solvedTheRiddle "sphinxTokens"
        li.validate violatedCausality "causalityTokens"
        li.validate weavedADreadCurse "dreadCurse"
        li "victoryXp"

  if withPlotChoice
    then resolutionFlavorWithChooseOne body $ for_ myriadPlans \(key, k) -> labeled key $ record k
    else resolutionFlavor body
