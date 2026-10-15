module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Scenarios.TheFallOfTheHouseOfUsher (
  theFallOfTheHouseOfUsher,
) where

import Arkham.Card.CardDef (CardDef)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Helpers
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Key
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement (Placement (InPlayArea))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Geist))

newtype TheFallOfTheHouseOfUsher = TheFallOfTheHouseOfUsher ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Republishes the hospitality log as a modifier, so a card's pure
@getAbilities@ can name it through 'investigatorWithHospitality'.
-}
instance HasModifiersFor TheFallOfTheHouseOfUsher where
  getModifiersFor (TheFallOfTheHouseOfUsher a) = modifySelectMapM a Anyone \iid -> do
    accepted <- hasAcceptedHospitality iid
    pure [ScenarioModifier acceptedTheUshersHospitalityKey | accepted]

theFallOfTheHouseOfUsher :: Difficulty -> TheFallOfTheHouseOfUsher
theFallOfTheHouseOfUsher difficulty =
  sideStory
    TheFallOfTheHouseOfUsher
    ":the-fall-of-the-house-of-usher:001"
    "The Fall of the House of Usher"
    difficulty
    [ "privateLibrary rodericksStudio  portraitGallery"
    , "banquetHall    grandHall        manorChapel"
    , "wineCellar     usherFamilyCrypt ."
    , "theTarn        .                ."
    ]

-- | "the number of [[Geist]] encounter cards in play" -- enemies, treacheries and assets alike.
geistCardsInPlay :: HasGame m => m Int
geistCardsInPlay = do
  enemies <- selectCount $ EnemyWithTrait Geist
  treacheries <- selectCount $ TreacheryWithTrait Geist <> InPlayTreachery
  assets <- selectCount $ AssetWithTrait Geist
  pure $ enemies + treacheries + assets

instance HasChaosTokenValue TheFallOfTheHouseOfUsher where
  getChaosTokenValue iid chaosTokenFace (TheFallOfTheHouseOfUsher attrs) = case chaosTokenFace of
    Skull -> do
      n <- geistCardsInPlay
      pure $ toChaosTokenValue attrs Skull (min 5 (1 + n)) (2 + n)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 0 1
    Tablet -> do
      damaged <- selectAny $ locationWithInvestigator iid <> LocationWithDamage (atLeast 2)
      pure $ toChaosTokenValue attrs Tablet (if damaged then 5 else 3) (if damaged then 6 else 4)
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

usherSetAsideCards :: [CardDef]
usherSetAsideCards =
  [ Assets.madelineUsher
  , Locations.theTarn
  , Enemies.theThingFromTheTarn
  , Assets.fadedSilhouette
  , Assets.theMadTristShelterFromTheStorm
  , Assets.theMadTristDragonsCunning
  ]

instance RunMessage TheFallOfTheHouseOfUsher where
  runMessage msg s@(TheFallOfTheHouseOfUsher attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheFallOfTheHouseOfUsher attrs do
      setup $ ul do
        li "gatherSets"
        li "setAside"
        li "placeLocations"
        li "roderickUsher"
        unscoped $ li "shuffleRemainder"

      gather Set.TheFallOfTheHouseOfUsher

      studio <- placeLabeled "rodericksStudio" Locations.rodericksStudio
      startAt studio
      placeLabeled_ "grandHall" Locations.grandHall
      placeLabeled_ "privateLibrary" Locations.privateLibrary
      placeLabeled_ "portraitGallery" Locations.portraitGallery
      placeLabeled_ "banquetHall" Locations.banquetHall
      placeLabeled_ "manorChapel" Locations.manorChapel
      placeLabeled_ "wineCellar" Locations.wineCellar
      placeLabeled_ "usherFamilyCrypt" Locations.usherFamilyCrypt

      assetAt_ Assets.roderickUsher studio

      setAside usherSetAsideCards

      setAgendaDeck [Agendas.hallOfAntiquity, Agendas.rushingAsunder]
      setActDeck [Acts.theDisturbedProprietor, Acts.fleetingGlimpses, Acts.ancestralCorruption]
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> do
          assets <- select $ assetControlledBy iid
          chooseTargetM iid assets $ placeDoomOn Cultist 1
        ElderThing -> assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          record TheHouseOfUsherHasFallen
          resolutionWithXp "resolution1" $ allGainXp' attrs
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsAvengedTheHouseOfUsher
          resolutionWithXp "resolution2" $ allGainXp' attrs
          offerReward Assets.fadedSilhouette
          for_ [Assets.theMadTristShelterFromTheStorm, Assets.theMadTristDragonsCunning] \madTrist ->
            whenM (selectAny $ assetIs madTrist) $ offerReward madTrist
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheFallOfTheHouseOfUsher <$> liftRunMessage msg attrs

{- | "Any one investigator may choose to add <card> to his or her deck. This card
does not count toward that investigator's deck size."
-}
offerReward :: (HasI18n, ReverseQueue m) => CardDef -> m ()
offerReward def = do
  lead <- getLead
  investigators <- getInvestigators
  chooseOneM lead do
    questionLabeled "addToDeck"
    questionLabeledCard def
    portraits investigators \owner -> do
      createAssetAt_ def (InPlayArea owner)
      addCampaignCardToDeck owner DoNotShuffleIn def
    labeled "declineReward" nothing

{- | The guide gives this scenario two difficulty modes only; Easy plays the
Standard bag and Expert the Hard one, which is also how its token effects pair.
-}
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Easy -> standardBag
  Standard -> standardBag
  Hard -> hardBag
  Expert -> hardBag
 where
  standardBag =
    [ PlusOne
    , PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , Skull
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
  hardBag =
    [ PlusOne
    , Zero
    , Zero
    , Zero
    , MinusOne
    , MinusOne
    , MinusOne
    , MinusTwo
    , MinusTwo
    , MinusThree
    , MinusThree
    , MinusFour
    , MinusFour
    , MinusFive
    , MinusSix
    , MinusEight
    , Skull
    , Skull
    , Skull
    , Cultist
    , Tablet
    , ElderThing
    , AutoFail
    , ElderSign
    ]
