{- | The Symphony of Erich Zann -- a fan side story.

The theatre is in two halves. The four front-of-house rooms are in play from
setup; the Backstage Rooms and the Stage Hall are set aside and only unlocked
when act 2 advances, which is also when the four Musician enemies are dealt out,
one to each room.

Every location prints its own symbol and connections, so the grid below is
layout only -- the engine wires the map from the symbols.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Scenarios.TheSymphonyOfErichZann (
  theSymphonyOfErichZann,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers
import Arkham.Homebrew.TheSymphonyOfErichZann.Key
import Arkham.Homebrew.TheSymphonyOfErichZann.Sets qualified as Set
import Arkham.Homebrew.TheSymphonyOfErichZann.Traits (pattern Music, pattern Musician)
import Arkham.Investigator.Types (Field (InvestigatorMentalTrauma))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Performer))

newtype TheSymphonyOfErichZann = TheSymphonyOfErichZann ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSymphonyOfErichZann :: Difficulty -> TheSymphonyOfErichZann
theSymphonyOfErichZann difficulty =
  sideStory
    TheSymphonyOfErichZann
    ":the-symphony-of-erich-zann:001"
    "The Symphony of Erich Zann"
    difficulty
    [ ".          entranceHall backstage1 backstage2"
    , "gallery    mainLobby    stageHall  ."
    , "auditorium .            backstage3 backstage4"
    ]

instance HasChaosTokenValue TheSymphonyOfErichZann where
  getChaosTokenValue iid chaosTokenFace (TheSymphonyOfErichZann attrs) = case chaosTokenFace of
    -- On Hard/Expert the skull is -X, where X is the number of [[Music]]
    -- treacheries in play.
    Skull -> do
      x <- length <$> musicTreacheriesInPlay
      pure $ toChaosTokenValue attrs Skull 1 x
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

-- | The six Backstage Rooms. Act 2 puts four into play and removes the other two.
backstageRooms :: [CardDef]
backstageRooms =
  [ Locations.anechoicChamber
  , Locations.instrumentCloset
  , Locations.recordingStudio
  , Locations.rehearsalRoom
  , Locations.sceneShop
  , Locations.tiringRoom
  ]

instance RunMessage TheSymphonyOfErichZann where
  runMessage msg s@(TheSymphonyOfErichZann attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "prologue" do
        flavor $ h "title" >> p "body"
        playingIsabel <- selectAny (InvestigatorWithTitle "Isabel La Fratta")
        when playingIsabel $ flavor $ scope "isabel" $ h "title" >> p "body"
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheSymphonyOfErichZann attrs do
      setup $ ul do
        li "gatherSets"
        li "setAsideEncounter"
        li "setAsideStory"
        li "setAsideMusicians"
        li "setAsideBeyondTheCurtain"
        li "setAsideBackstage"
        li "placeLocations"
        li "performerWeakness"
        unscoped $ li "shuffleRemainder"

      additionalRules "musicTreacheries"

      gather Set.TheSymphonyOfErichZann

      -- Front of house. Every connection is printed, so nothing is wired here.
      startAt =<< placeLabeled "entranceHall" Locations.entranceHall
      placeLabeled_ "mainLobby" Locations.mainLobby
      placeLabeled_ "gallery" Locations.gallery
      placeLabeled_ "auditorium" Locations.auditorium

      setAside
        $ [ Enemies.earsOfTheVoid
          , Enemies.earsOfTheVoid
          , Treacheries.heardBySomething
          , Treacheries.heardBySomething
          , Treacheries.heardBySomething
          , Enemies.youngNightingale
          , Assets.augusteGaudinMaestroOfSymphonies
          , Assets.yinsDrumsticks
          , Assets.pagesViolin
          , Assets.laFrattasPianoKey
          , Assets.walkersTrumpet
          , Assets.thePiano
          , Treacheries.stuckInYourHead
          , Treacheries.stuckInYourHead
          , Treacheries.stuckInYourHead
          , Treacheries.stuckInYourHead
          , Stories.beyondTheCurtain
          , Locations.stageHall
          ]
        <> backstageRooms
        <> musicians

      -- "Each Performer investigator begins play with a copy of the set aside
      -- Stuck in Your Head treachery in their hand."
      performers <- select $ InvestigatorWithTrait Performer
      for_ performers \iid -> do
        card <- fromSetAside Treacheries.stuckInYourHead
        addToHand iid [card]

      setAgendaDeck
        [ Agendas.overture
        , Agendas.crescendo
        , Agendas.opusMagnum
        , Agendas.codaUltimatum
        ]
      setActDeck
        [ Acts.musicFromAuseilTheatre
        , Acts.thePossessedConductor
        , Acts.undreamableOrchestra
        ]
    -- "[skull]: Reveal another token."
    ResolveChaosToken _ Skull iid -> do
      drawAnotherChaosToken iid
      pure s
    {- "[cultist]: After this test ends, discard cards from the top of the
    encounter deck until a [[Music]] treachery is discarded. Draw it." -}
    ResolveChaosToken _ Cultist iid -> do
      afterMaybeSkillTestQuiet
        $ discardUntilFirst iid attrs Deck.EncounterDeck (basic $ #treachery <> withTrait Music)
      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      drawCard iid card
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        {- "[tablet]: If you fail, place 1 doom on a Musician enemy at your
        location." On Hard/Expert it is instead the /nearest/ Musician enemy,
        which can be a tie, so both readings ask. -}
        Tablet -> do
          candidates <-
            select
              $ if isEasyStandard attrs
                then EnemyWithTrait Musician <> enemyAtLocationWith iid
                else NearestEnemyTo iid (EnemyWithTrait Musician)
          chooseTargetM iid candidates $ placeDoomOn Tablet 1
        {- "[elder thing]: If you fail, each ready Musician enemy at your
        location immediately engages and attacks you." -}
        ElderThing -> do
          enemies <- select $ EnemyWithTrait Musician <> ReadyEnemy <> enemyAtLocationWith iid
          for_ enemies \enemy -> do
            engageEnemy iid enemy
            initiateEnemyAttack enemy ElderThing iid
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): Proceed to Resolution 1." It delegates entirely, so the
        defeat step and the ending both belong to Resolution 1. -}
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          -- "Before resolving any other resolution, if at least 1 investigator
          -- was defeated: the defeated investigators read Investigator Defeat first."
          investigatorDefeat
          record AllIsQuietAtRueDAuseilForNow
          resolutionWithXp "resolution1" $ allGainXp' attrs

          -- "Each investigator who was not defeated may remove the Stuck in
          -- Your Head weakness from their deck."
          survivors <- select $ not_ DefeatedInvestigator
          for_ survivors \iid -> do
            hasWeakness <-
              selectAny
                $ InvestigatorWithId iid
                <> DeckWith (HasCard $ cardIs Treacheries.stuckInYourHead)
            when hasWeakness $ chooseOneM iid do
              labeled "removeStuckInYourHead"
                $ removeCampaignCardFromDeck iid Treacheries.stuckInYourHead
              labeled "keepStuckInYourHead" nothing

          {- "If you 'saved all the musicians', each investigator may either heal
          1 mental trauma, or earn 1 additional experience." Healing is only
          offered to someone who has mental trauma to heal. -}
          savedAll <- getHasRecord YouSavedAllTheMusicians
          when savedAll $ eachInvestigator \iid -> do
            hasMental <- fieldP InvestigatorMentalTrauma (> 0) iid
            chooseOneM iid do
              when hasMental
                $ labeled "healMentalTrauma"
                $ push
                $ HealTrauma iid 0 1
              labeled "gainExperience"
                $ gainXp iid attrs (ikey "xp.savedAllTheMusicians") 1

          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheSymphonyOfErichZann <$> liftRunMessage msg attrs

{- | "Before resolving any other resolution, if at least 1 investigator was
defeated: The defeated investigators must read Investigator Defeat first."
-}
investigatorDefeat :: (HasI18n, ReverseQueue m) => m ()
investigatorDefeat = do
  defeated <- select DefeatedInvestigator
  unless (null defeated) do
    resolutionOnly defeated $ withTitle "investigatorDefeat"
    for_ defeated \iid -> do
      {- "Each investigator who was defeated and does not already have a copy of
      the Stuck in your Head weakness in their deck must add 1 copy of it to his
      or her deck. This card does not count toward deck size." -}
      hasWeakness <-
        selectAny
          $ InvestigatorWithId iid
          <> DeckWith (HasCard $ cardIs Treacheries.stuckInYourHead)
      unless hasWeakness
        $ addCampaignCardToDeck iid DoNotShuffleIn Treacheries.stuckInYourHead

      -- "...suffers 1 mental trauma for having listened from within the void."
      sufferMentalTrauma iid 1

      {- "If an investigator with Auguste Gaudin (Maestro of Symphonies), Yin's
      Drumsticks, Page's Violin, La Fratta's Piano Key or Walker's Trumpet was
      defeated, that card must be removed from that investigator's deck." -}
      for_ earnedRewards $ removeCampaignCardFromDeck iid
 where
  earnedRewards =
    [ Assets.augusteGaudinMaestroOfSymphonies
    , Assets.yinsDrumsticks
    , Assets.pagesViolin
    , Assets.laFrattasPianoKey
    , Assets.walkersTrumpet
    ]

chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy ->
    [ PlusOne
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Standard ->
    [ PlusOne
    , Zero
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Hard ->
    [ Zero
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  Expert ->
    [ Zero
    , MinusOne
    , MinusTwo
    , MinusThree
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
