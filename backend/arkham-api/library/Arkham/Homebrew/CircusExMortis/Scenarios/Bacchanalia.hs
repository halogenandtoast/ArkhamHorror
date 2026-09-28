module Arkham.Homebrew.CircusExMortis.Scenarios.Bacchanalia (bacchanalia) where

import Arkham.Card
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Helpers.SkillTest (getSkillTestAction, isParley)
import Arkham.Helpers.Xp (XpBonus (NoBonus))
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n
import Arkham.Id (InvestigatorId)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (SilverTwilight, Socialite))
import Arkham.Trait qualified as Trait
import Arkham.Treachery.CardDefs.CurseOfTheRougarou qualified as TreacheryCards

newtype Bacchanalia = Bacchanalia ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bacchanalia :: Difficulty -> Bacchanalia
bacchanalia difficulty =
  scenario
    Bacchanalia
    ":circus-ex-mortis:122"
    "Bacchanalia"
    difficulty
    [ ". . upperBalcony upperBalcony . ."
    , ". privateParlor privateParlor collectionHall collectionHall ."
    , "banquetHall banquetHall vestibule vestibule statuaryGardens statuaryGardens"
    , "manorCellars manorCellars . . hiddenDungeon hiddenDungeon"
    , ". . savageAltar savageAltar . ."
    ]

-- | The five Socialite story assets, one per location other than Vestibule.
socialites :: [CardDef]
socialites =
  [ Assets.cecilSharpe
  , Assets.estherMeredith
  , Assets.phillipHutchins
  , Assets.richardStratton
  , Assets.veraAshcroft
  ]

-- | Placed at setup; the three @Restricted@ locations arrive with act 1's back.
manorLocations :: [CardDef]
manorLocations =
  [ Locations.banquetHall
  , Locations.statuaryGardens
  , Locations.privateParlor
  , Locations.collectionHall
  , Locations.upperBalcony
  ]

{- | "If the Curse of the Rougarou is in your deck, you must choose this option."
The campaign upgrades the official weakness to its own printing in scenario IV,
and either one may already have been drawn into the threat area.
-}
isCursed :: HasGame m => InvestigatorId -> m Bool
isCursed iid =
  orM
    [ iid <=~> DeckWith (HasCard $ oneOf [cardIs curse | curse <- curses])
    , selectAny $ treacheryInThreatAreaOf iid <> oneOf [treacheryIs curse | curse <- curses]
    ]
 where
  curses = [TreacheryCards.curseOfTheRougarou, Treacheries.curseOfTheRougarou]

-- | Who reads /Upper Echelons/ in the intro, and starts with 2 extra resources.
upperEchelonsInvestigators :: InvestigatorMatcher
upperEchelonsInvestigators =
  oneOf [InvestigatorWithTrait Socialite, InvestigatorWithTrait SilverTwilight]

viceLabel :: Vice -> Text
viceLabel = \case
  Revelry -> "revelry"
  Intimacy -> "intimacy"
  Opulence -> "opulence"
  Violence -> "violence"

{- | "Each investigator may choose any amount of the following." A cursed
investigator has no Done button until they have taken the vice for violence, so
the forced pick is visible rather than silently applied.
-}
chooseVices :: (HasI18n, ReverseQueue m) => InvestigatorId -> Bool -> [Vice] -> m ()
chooseVices iid mustTakeViolence remaining = do
  let canFinish = not mustTakeViolence || Violence `notElem` remaining
  unless (null remaining) $ chooseOneM iid do
    when canFinish $ labeled "done" (pure ())
    for_ remaining \v -> labeled (viceLabel v) do
      recordVice iid v
      chooseVices iid mustTakeViolence (filter (/= v) remaining)

instance HasModifiersFor Bacchanalia where
  getModifiersFor (Bacchanalia a) = modifySelectMapM a Anyone \iid -> do
    vices <- getVices iid
    pure
      $ map (ScenarioModifier . viceKey) vices
      <> [XPModifier viceXpLabel (length vices) | notNull vices]

-- | "1 bonus experience for each vice they picked at the start of the scenario."
viceXpLabel :: Text
viceXpLabel = scenarioI18n "bacchanalia" $ scope "xp" $ "$" <> ikey "vices"

instance HasChaosTokenValue Bacchanalia where
  getChaosTokenValue iid tokenFace (Bacchanalia attrs) = case tokenFace of
    Skull -> do
      x <- (`divideRoundUp` 2) <$> getViceCount iid
      pure $ toChaosTokenValue attrs Skull x (x + 1)
    Cultist -> do
      investigating <- (== Just #investigate) <$> getSkillTestAction
      pure
        $ if investigating
          then toChaosTokenValue attrs Cultist 4 5
          else toChaosTokenValue attrs Cultist 2 3
    Tablet -> do
      cultistHere <- selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Trait.Cultist
      pure
        $ if cultistHere
          then toChaosTokenValue attrs Tablet 4 5
          else toChaosTokenValue attrs Tablet 2 3
    ElderThing -> do
      parleying <- isParley
      pure
        $ if parleying
          then toChaosTokenValue attrs ElderThing 4 5
          else toChaosTokenValue attrs ElderThing 2 3
    MoonToken -> pure moonTokenValue
    otherFace -> getChaosTokenValue iid otherFace attrs

divideRoundUp :: Int -> Int -> Int
divideRoundUp n d = (n + d - 1) `div` d

instance RunMessage Bacchanalia where
  runMessage msg s@(Bacchanalia attrs) = runQueueT $ scenarioI18n "bacchanalia" $ case msg of
    PreScenarioSetup -> scope "intro" do
      upperEchelons <- selectAny upperEchelonsInvestigators
      flavor do
        h "title"
        p "body1"
        p "body2"
        p.green.validate upperEchelons "upperEchelons"
        p "body3"
        ul $ li.nested "choose" do
          li "revelry"
          li "intimacy"
          li "opulence"
          li "violence"
      eachInvestigator \iid -> do
        mustTakeViolence <- isCursed iid
        chooseVices iid mustTakeViolence allVices
      pure s
    Setup -> runScenarioSetup Bacchanalia attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "socialites"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.Bacchanalia
      gather Set.CultOfShubNiggurath
      gather Set.NewMoonEntertainers
      gather Set.PanickedMasses
      gather Set.PrimordialEvils

      others <- placeAllCapture manorLocations
      startAt =<< place Locations.vestibule

      n <- getPlayerCount
      shuffled <- shuffle socialites
      for_ (zip shuffled others) \(def, lid) -> do
        aid <- createAssetAt def (AtLocation lid)
        placeTokens attrs aid Token.Clue n

      setAside
        [ Assets.terrifiedCaptives
        , Assets.terrifiedCaptives
        , Treacheries.wildHysteria
        , Treacheries.wildHysteria
        , Locations.hiddenDungeon
        , Locations.manorCellars
        , Locations.savageAltar
        , Enemies.goatspawnCorruptor
        ]

      setAgendaDeck [Agendas.intoTheLionsDen, Agendas.lackOfRestraint, Agendas.feverPitch]
      setActDeck [Acts.behindClosedDoors, Acts.deeperProfanities, Acts.fashionablyEarly]

      -- Upper Echelons: "You begin this scenario with 2 additional resources."
      selectEach upperEchelonsInvestigators \iid -> gainResources iid attrs 2
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          act <- getCurrentActStep
          anyResigned <- selectAny ResignedInvestigator
          push $ if not anyResigned && act < 3 then R1 else R2
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsMustFollowTheCult
          push R3
        Resolution 2 -> do
          resolution "resolution2"
          record TheInvestigatorsDiscoveredTheRitualsLocation
          push R3
        Resolution 3 -> do
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs NoBonus
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> Bacchanalia <$> liftRunMessage msg attrs
