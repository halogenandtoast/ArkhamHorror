module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Scenarios.TheStrangeCaseOfDrJekyllAndMrHyde (
  theStrangeCaseOfDrJekyllAndMrHyde,
) where

import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Card.CardDef (CardDef)
import Arkham.Difficulty
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (withSkillTestTargetedEnemy)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Investigators qualified as Investigators
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Key
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Message.Lifted.Placement qualified as Placement
import Arkham.Placement (Placement (AtLocation))
import Arkham.Projection (field)
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Sorcerer))

newtype TheStrangeCaseOfDrJekyllAndMrHyde = TheStrangeCaseOfDrJekyllAndMrHyde ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | The suggested placement from the scenario card: Northside Plaza in the
middle of its eight neighbours, with the arrangement act 1 lays out running
along the bottom.
-}
theStrangeCaseOfDrJekyllAndMrHyde :: Difficulty -> TheStrangeCaseOfDrJekyllAndMrHyde
theStrangeCaseOfDrJekyllAndMrHyde difficulty =
  sideStory
    TheStrangeCaseOfDrJekyllAndMrHyde
    ":the-strange-case-of-dr-jekyll-and-mr-hyde:001"
    "The Strange Case of Dr. Jekyll and Mr. Hyde"
    difficulty
    [ ".            .                tannery        trainyard     ."
    , ".            chemicalFirm     northsidePlaza drugstore     ."
    , ".            abandonedFactory etchersShop    importsOffice ."
    , "arrangement1 arrangement2     arrangement3   arrangement4  arrangement5"
    ]

{- | "When placing locations, randomly choose one of each duplicate location to
put into play and set the other aside, out of play." The pair that stays out is
what act 1 shuffles into the arrangement.
-}
duplicates :: [(Text, (CardDef, CardDef))]
duplicates =
  [ ("drugstore", (Locations.drugstore_008, Locations.drugstore_009))
  , ("importsOffice", (Locations.importsOffice_010, Locations.importsOffice_011))
  , ("etchersShop", (Locations.etchersShop_012, Locations.etchersShop_013))
  , ("chemicalFirm", (Locations.chemicalFirm_014, Locations.chemicalFirm_015))
  , ("tannery", (Locations.tannery_016, Locations.tannery_017))
  ]

{- | "Investigators cannot move to or reveal these locations, but they are
considered to be in play for the purpose of other game effects." The arrangement
holds the second printing of a place already on the board, so without
'DoesNotConnect' the map would wire itself to both.
-}
instance HasModifiersFor TheStrangeCaseOfDrJekyllAndMrHyde where
  getModifiersFor (TheStrangeCaseOfDrJekyllAndMrHyde a) =
    modifySelect a inArrangement [Blocked, CannotBeRevealed, DoesNotConnect]

instance HasChaosTokenValue TheStrangeCaseOfDrJekyllAndMrHyde where
  getChaosTokenValue iid chaosTokenFace (TheStrangeCaseOfDrJekyllAndMrHyde attrs) = case chaosTokenFace of
    Skull -> do
      -- "-1. -3 instead if Edward Hyde is in play."
      hyde <- edwardHydeInPlay
      pure $ toChaosTokenValue attrs Skull (if hyde then 3 else 1) (if hyde then 4 else 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheStrangeCaseOfDrJekyllAndMrHyde where
  runMessage msg s@(TheStrangeCaseOfDrJekyllAndMrHyde attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheStrangeCaseOfDrJekyllAndMrHyde attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setAside"
        li "jekyll"
        li "doom"
        unscoped $ li "shuffleRemainder"

      gather Set.TheStrangeCaseOfDrJekyllAndMrHyde

      setAside
        [ Stories.adaptedFormula
        , Assets.fundamentalFormula
        , Assets.duplicitousDrug
        , Investigators.yourOtherHalf
        ]

      plaza <- placeLabeled "northsidePlaza" Locations.northsidePlaza
      startAt plaza
      placeLabeled_ "trainyard" Locations.trainyard
      placeLabeled_ "abandonedFactory" Locations.abandonedFactory
      for_ duplicates \(lbl, (a, b)) ->
        shuffleM [a, b] >>= \case
          (onBoard : aside) -> placeLabeled_ lbl onBoard >> setAside aside
          [] -> pure ()

      createAssetAt_ Assets.henryJekyll (AtLocation plaza)
      placeDoomOnAgenda 1

      setAgendaDeck [Agendas.theDualityOfMan]
      setActDeck
        [Acts.theMadmansNotebook, Acts.essentialMaterials, Acts.tasteOfYourOwnMedicine]
    ResolveChaosToken _ Cultist iid -> do
      -- "If this is a test against a Sorcerer enemy, take 1 horror."
      withSkillTestTargetedEnemy \enemy ->
        whenM (enemy <=~> EnemyWithTrait Sorcerer) $ assignHorror iid Cultist 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      -- "If this is a test as part of an {action} or {fast} ability printed on
      -- a scenario card, take 1 damage."
      whenM testIsFromScenarioActionOrFastAbility $ assignDamage iid Tablet 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == ElderThing -> do
      -- "move Henry Jekyll one location away from you, or Edward Hyde one
      -- location away from you if he is unengaged." They are two faces of the
      -- same card, so at most one of them is ever on the table.
      selectOne henryJekyll >>= \case
        -- Jekyll is an asset, and only enemies and investigators answer `Move`,
        -- so he is placed a step further out instead.
        Just jekyll -> whenJustM (field AssetLocation jekyll) \lid -> do
          dests <-
            select
              $ connectedFrom (LocationWithId lid)
              <> LocationFartherFrom lid (locationWithInvestigator iid)
          chooseTargetM iid dests $ Placement.place jekyll . AtLocation
        Nothing ->
          selectOne (edwardHyde <> UnengagedEnemy) >>= traverse_ \hyde ->
            moveTowardsMatching
              ElderThing
              hyde
              (FarthestLocationFromInvestigator (InvestigatorWithId iid) Anywhere)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      investigators <- allInvestigators
      chemistTaken <- selectAny $ VictoryDisplayCardMatch (basic $ cardIs Enemies.sinisterChemist)
      case r of
        NoResolution -> push $ ScenarioResolution $ Resolution 1
        Resolution 1 -> do
          record MrHydeEscapedYourGrasp
          resolutionWithXp "resolution1" $ allGainXp' attrs
          when chemistTaken
            $ addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.fundamentalFormula
          endOfScenario
        Resolution 2 -> do
          record TheInvestigatorsDestroyedMrHyde
          resolutionWithXp "resolution2" $ allGainXp' attrs
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.duplicitousDrug
          when chemistTaken
            $ addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.fundamentalFormula
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheStrangeCaseOfDrJekyllAndMrHyde <$> liftRunMessage msg attrs

{- | Played standalone, this scenario has two difficulty modes; Easy plays the
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
