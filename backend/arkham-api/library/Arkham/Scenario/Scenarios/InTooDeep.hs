module Arkham.Scenario.Scenarios.InTooDeep (InTooDeep (..), inTooDeep) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Helpers.Location (getConnectedLocations, getLocationOf)
import Arkham.Helpers.Log (getCircledRecord, getRecordSet)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMaybe )
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (metaL)
import Arkham.ScenarioLogKey
import Arkham.Scenarios.InTooDeep.Helpers hiding (setBarriers)
import Arkham.Scenarios.InTooDeep.Helpers qualified as Helpers
import Arkham.SortedPair
import Control.Lens (use)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

newtype InTooDeep = InTooDeep ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

getBlockedFrom :: HasGame m => Meta -> LocationId -> m [LocationId]
getBlockedFrom (Meta meta) lid = do
  let
    unblocked x y = Map.findWithDefault 0 (sortedPair y x) meta == 0
    bfs visited [] = pure $ toList visited
    bfs visited (current : queue)
      | current `Set.member` visited = bfs visited queue
      | otherwise = do
          next <- filterBy [unblocked current, (`Set.notMember` visited)] <$> getConnectedLocations current
          bfs (Set.insert current visited) (queue <> next)
  select . not_ . beOneOf @_ @LocationMatcher  =<< bfs Set.empty [lid]

instance HasModifiersFor InTooDeep where
  getModifiersFor (InTooDeep a) = do
    investigators <- modifySelectMaybe a Anyone \iid -> do
      lid <- MaybeT $ getLocationOf iid
      meta <- hoistMaybe $ maybeResult @Meta a.meta
      CannotEnter <$$> lift (getBlockedFrom meta lid)
    locations <- modifySelectMaybe a Anywhere \lid -> do
      Meta meta <- hoistMaybe $ maybeResult @Meta a.meta
      let
        barricaded (pair, n) = case unSortedPair pair of
          (l1, l2) | l1 == lid -> guard (n > 0) $> l2
          (l1, l2) | l2 == lid -> guard (n > 0) $> l1
          _ -> Nothing
      let barriers = mapMaybe barricaded $ Map.toList meta
      pure [Barricades barriers | notNull barriers]
    enemies <- modifySelect a (not_ $ enemyIs Enemies.innsmouthShoggoth) [CanIgnoreBarriers]
    pure $ investigators <> locations <> enemies

inTooDeep :: Difficulty -> InTooDeep
inTooDeep difficulty = scenario InTooDeep "07123" "In Too Deep" difficulty []

instance HasChaosTokenValue InTooDeep where
  getChaosTokenValue iid tokenFace (InTooDeep attrs) = case tokenFace of
    Skull -> do
      n <-
        fromMaybe 0 <$> runMaybeT do
          loc <- MaybeT $ getLocationOf iid
          Pos x _ <- hoistMaybe $ findInGrid loc attrs.grid
          pure $ abs x
      pure $ toChaosTokenValue attrs Skull n (n * 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> do
      x <-
        fromMaybe 0 <$> runMaybeT do
          loc <- MaybeT $ getLocationOf iid
          Meta meta <- hoistMaybe $ maybeResult @Meta attrs.meta
          ls <- lift $ getConnectedLocations loc
          pure $ sum [Map.findWithDefault 0 (sortedPair loc l) meta | l <- ls]
      pure $ toChaosTokenValue attrs ElderThing x (x * 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

setBarriers :: ReverseQueue m => LocationId -> LocationId -> Int -> ScenarioBuilderT m ()
setBarriers a b n = do
  meta <- toResultDefault (Meta mempty) <$> use (attrsL . metaL)
  setMeta $ Helpers.setBarriers a b n meta

instance RunMessage InTooDeep where
  runMessage msg s@(InTooDeep attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      hideout <- sample $ InnsmouthJail :| [ ShorewardSlums , SawboneAlley , TheHouseOnWaterStreet , EsotericOrderOfDagon , NewChurchGreen ]

      recordSetInsert PossibleSuspects
        $ map toJSON [BrianBurnham, BarnabasMarsh, OtheraGilman, ZadokAllen, JoyceLittle, RobertFriendly]
      recordSetInsert PossibleHideouts
        $ map
          toJSON
          [ InnsmouthJail
          , ShorewardSlums
          , SawboneAlley
          , TheHouseOnWaterStreet
          , EsotericOrderOfDagon
          , NewChurchGreen
          ]
      recordSetReplace PossibleHideouts (recorded $ toJSON hideout) (circled $ toJSON hideout)
      pure s
    Setup -> runScenarioSetup InTooDeep attrs do
      setUsesGrid

      gather Set.InTooDeep
      gather Set.CreaturesOfTheDeep
      gather Set.RisingTide
      gather Set.Syzygy
      gather Set.TheLocals
      gather Set.AgentsOfCthulhu

      setAgendaDeck
        [ Agendas.barricadedStreets
        , Agendas.relentlessTide
        , Agendas.floodedStreets
        , Agendas.rageOfTheDeep
        ]
      setActDeck [Acts.throughTheLabyrinth]

      -- bottom row
      desolateCoastline <- placeInGrid (Pos 0 0) Locations.desolateCoastline
      shorewardSlums <- placeInGrid (Pos (-1) 0) Locations.shorewardSlumsInTooDeep
      innsmouthJail <- placeInGrid (Pos (-2) 0) Locations.innsmouthJailInTooDeep
      gilmanHouse <- placeInGrid (Pos (-3) 0) Locations.gilmanHouseInTooDeep
      sawboneAlley <- placeInGrid (Pos (-4) 0) Locations.sawboneAlleyInTooDeep

      -- middle row
      innsmouthHarbour <- placeInGrid (Pos 0 1) Locations.innsmouthHarbourInTooDeep
      fishStreetBridge <- placeInGrid (Pos (-1) 1) Locations.fishStreetBridgeInTooDeep
      innsmouthSquare <- placeInGrid (Pos (-2) 1) Locations.innsmouthSquareInTooDeep
      firstNationalGrocery <- placeInGrid (Pos (-3) 1) Locations.firstNationalGroceryInTooDeep
      theLittleBookshop <- placeInGrid (Pos (-4) 1) Locations.theLittleBookshopInTooDeep

      -- top row
      theHouseOnWaterStreet <- placeInGrid (Pos 0 2) Locations.theHouseOnWaterStreetInTooDeep
      marshRefinery <- placeInGrid (Pos (-1) 2) Locations.marshRefineryInTooDeep
      newChurchGreen <- placeInGrid (Pos (-2) 2) Locations.newChurchGreenInTooDeep
      esotericOrderOfDagon <- placeInGrid (Pos (-3) 2) Locations.esotericOrderOfDagonInTooDeep
      railroadStation <- placeInGrid (Pos (-4) 2) Locations.railroadStation

      -- bottom row
      setBarriers desolateCoastline shorewardSlums 2
      setBarriers shorewardSlums innsmouthJail 1
      setBarriers innsmouthJail gilmanHouse 3
      setBarriers gilmanHouse sawboneAlley 1

      -- middle row
      setBarriers innsmouthHarbour fishStreetBridge 1
      setBarriers fishStreetBridge innsmouthSquare 2
      setBarriers innsmouthSquare firstNationalGrocery 2
      setBarriers firstNationalGrocery theLittleBookshop 2
      setBarriers theLittleBookshop railroadStation 1

      -- top row
      setBarriers theHouseOnWaterStreet marshRefinery 1
      setBarriers marshRefinery newChurchGreen 3
      setBarriers newChurchGreen esotericOrderOfDagon 1
      setBarriers esotericOrderOfDagon railroadStation 4

      startAt desolateCoastline

      setAsideKeys $ map UnrevealedKey [RedKey, BlueKey, GreenKey, YellowKey, PurpleKey, WhiteKey]

      mHideout <- maybeResult <$$> getCircledRecord PossibleHideouts
      for_ (join mHideout) \hideout -> do
        let
          hideoutLocation = case hideout of
            InnsmouthJail -> innsmouthJail
            ShorewardSlums -> shorewardSlums
            SawboneAlley -> sawboneAlley
            TheHouseOnWaterStreet -> theHouseOnWaterStreet
            EsotericOrderOfDagon -> esotericOrderOfDagon
            NewChurchGreen -> newChurchGreen
        placeKey hideoutLocation BlackKey

      outForBlood <- mapMaybe (maybeResult <=< unrecorded) <$> getRecordSet OutForBlood
      for_ outForBlood \case
        BrianBurnham -> enemyAt_ Enemies.brianBurnhamWantsOut firstNationalGrocery
        BarnabasMarsh -> enemyAt_ Enemies.barnabasMarshTheChangeIsUponHim marshRefinery
        OtheraGilman -> enemyAt_ Enemies.otheraGilmanProprietessOfTheHotel gilmanHouse
        ZadokAllen -> enemyAt_ Enemies.zadokAllenDrunkAndDisorderly fishStreetBridge
        JoyceLittle -> enemyAt_ Enemies.joyceLittleBookshopOwner theLittleBookshop
        RobertFriendly -> enemyAt_ Enemies.robertFriendlyDisgruntledDockworker innsmouthHarbour

      setAside
        [ Enemies.ravagerFromTheDeep
        , Enemies.ravagerFromTheDeep
        , Enemies.youngDeepOne
        , Enemies.youngDeepOne
        , Assets.joeSargentRattletrapBusDriver
        , Assets.teachingsOfTheOrder
        , Enemies.innsmouthShoggoth
        , Enemies.angryMob
        ]

      for_ [theHouseOnWaterStreet, innsmouthHarbour, desolateCoastline] (push . IncreaseFloodLevel)
    ScenarioCountIncrementBy (Barriers l1 l2) n -> do
      desolateCoastline <- selectJust $ locationIs Locations.desolateCoastline
      if l1 == desolateCoastline || l2 == desolateCoastline
        then pure s
        else do
          let meta' = incrementBarriers n l1 l2 $ toResultDefault (Meta mempty) attrs.meta
          pure $ InTooDeep $ attrs & metaL .~ toJSON meta'
    ScenarioCountDecrementBy (Barriers l1 l2) n -> do
      let meta' = decrementBarriers n l1 l2 $ toResultDefault (Meta mempty) attrs.meta
      pure $ InTooDeep $ attrs & metaL .~ toJSON meta'
    ScenarioCountSet (Barriers l1 l2) n -> do
      let meta' = Helpers.setBarriers l1 l2 n $ toResultDefault (Meta mempty) attrs.meta
      pure $ InTooDeep $ attrs & metaL .~ toJSON meta'
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> void $ runMaybeT do
          lid <- MaybeT $ getLocationOf iid
          pos <- hoistMaybe $ findInGrid lid attrs.grid
          GridLocation _ east <- hoistMaybe $ viewGrid (updatePosition pos East) attrs.grid
          lift $ moveTo Cultist iid east
        Tablet -> withLocationOf iid \lid -> do
          connected <- getConnectedLocations lid
          let Meta meta = toResultDefault (Meta mempty) attrs.meta
          let choices = filter (\l -> Map.findWithDefault 0 (sortedPair lid l) meta == 0) connected
          chooseTargetM iid choices $ \l -> push $ ScenarioCountIncrementBy (Barriers lid l) 1
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          push R1
        Resolution 1 -> do
          story $ i18nWithTitle "resolution1"
          madeItSafely <- getHasRecord TheInvestigatorsMadeItSafelyToTheirVehicles
          allGainXpWithBonus attrs
            $ if madeItSafely then WithBonus "Made it safely to their vehicles" 2 else NoBonus
          endOfScenario
        _ -> throw $ UnknownResolution resolution
      pure s
    _ -> InTooDeep <$> liftRunMessage msg attrs
