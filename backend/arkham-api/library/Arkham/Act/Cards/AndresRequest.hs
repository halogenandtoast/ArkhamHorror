module Arkham.Act.Cards.AndresRequest (andresRequest) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Token
import Arkham.Trait (Trait (Prop, Set))

newtype AndresRequest = AndresRequest ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

andresRequest :: ActCard AndresRequest
andresRequest = act (1, A) AndresRequest Cards.andresRequest Nothing

instance HasModifiersFor AndresRequest where
  getModifiersFor (AndresRequest a) = do
    modifySelect a (EnemyWithTitle "Possessed Extra") [AddKeyword Aloof]

instance HasAbilities AndresRequest where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (InVictoryDisplay (#asset <> withTrait Prop) (atLeast 2))
        $ Objective
        $ triggered (RoundEnds #when) Free
    ]

instance RunMessage AndresRequest where
  runMessage msg a@(AndresRequest attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n do
      eachInvestigator (discardAllClues attrs)
      centralLot <- selectJust $ LocationWithTitle "Central Lot"
      cards <- field LocationCardsUnderneath centralLot
      for_ cards setCardAside
      investigators <- select UneliminatedInvestigator
      leadChooseOneM do
        questionLabeled' "andresRequest.takeAndre"
        questionLabeledCard Assets.andrePatelMadeForTheSpotlight
        targets investigators \iid -> do
          andres <- createAsset =<< getSetAsideCard Assets.andrePatelMadeForTheSpotlight
          gameModifier attrs andres (DoNotTakeUpSlot #ally)
          takeControlOfAsset iid andres

      void $ runMaybeT do
        let getProp = MaybeT . selectOne . VictoryDisplayCardMatch . basic . cardIs
        telescope <- getProp Assets.heliosTelescopeGateToTheCosmos
        staff <- getProp Assets.staffOfTheSerpentRelicOfThePast
        cape <- getProp Assets.accursedCapeShroudOfChaos

        lift do
          addChaosToken #"0"
          leadChooseOneM do
            questionLabeled' "andresRequest.setAside"
            cardLabeled telescope $ setCardAside telescope
            cardLabeled staff $ setCardAside staff
            cardLabeled cape $ setCardAside cape

      doStep 2 msg
      advanceActDeck attrs
      pure a
    DoStep 2 (AdvanceAct (isSide B attrs -> True) _ _) -> scenarioI18n $ scope "interlude" do
      mtelescope <-
        selectOne $ VictoryDisplayCardMatch (basic $ cardIs Assets.heliosTelescopeGateToTheCosmos)
      mstaff <-
        selectOne $ VictoryDisplayCardMatch (basic $ cardIs Assets.staffOfTheSerpentRelicOfThePast)
      mcape <- selectOne $ VictoryDisplayCardMatch (basic $ cardIs Assets.accursedCapeShroudOfChaos)

      storyWithContinue' do
        p "instructions"
        ul do
          li.validate (isNothing mtelescope) "heliosTelescope"
          li.validate (isNothing mstaff) "staffOfTheSerpent"
          li.validate (isNothing mcape) "accursedCape"

      discardEach attrs AnyEnemy

      when (isNothing mtelescope) do
        storyWithContinue' $ scope "aCosmicJourney" do
          p "body"
          ul do
            li "gatherSets"
            li "set"
            li.nested "locations" $ li "rocketShip"
            li "placeClues"
            li "saturniteMonarch"
            li "acts"
            li "storyAsset"
            li "shuffleRemaining"
            li "reelDeck"
            li "tokens"
            li "continue"
        spaceSet <- selectJust $ locationIs Locations.spaceSet
        eachInvestigator \iid -> moveTo attrs iid spaceSet
        selectEach (LocationWithTrait Set <> not_ (be spaceSet)) \other -> do
          removeLocation other
          setCardAside =<< field LocationCard other
        highRulersBastion <- placeLocation Locations.highRulersBastion
        placeLocationCards
          [ Locations.teetawnPassage
          , Locations.ritualSiteTeetawn
          , Locations.tothisBarrens
          , Locations.ritualSiteTothis
          ]
        push
          $ SetLayout
            [ "lostAsteroid1     spaceSet          lostAsteroid2"
            , "teetawnPassage    highRulersBastion tothisBarrens"
            , "ritualSiteTeetawn .                 ritualSiteTothis"
            ]
        rocketShip <- createAssetAt Assets.rocketShipRattlingWithEnergy (AtLocation spaceSet)
        eachInvestigator \iid -> push . PlaceInvestigator iid $ InVehicle rocketShip
        n <- fieldMapM LocationRevealClues getGameValue spaceSet
        push $ PlaceCluesUpToClueValue spaceSet (toSource attrs) n
        createEnemyAt_ Enemies.saturniteMonarchGraciousHost highRulersBastion
        setActDeck [Cards.andresRequest, Cards.aCosmicJourney, Cards.fromWhenceTheyCame]
        createAssetAt_ Assets.heliosTelescopeGateToTheCosmos Global
        shuffleSetAsideEncounterSetIntoEncounterDeck EncounterSet.CosmicJourney
        shuffleEncounterDiscardBackIn
        shuffleSetAsideIntoScenarioDeck ReelDeck
          $ SingleSidedCard
          <> fromSets [EncounterSet.ForgottenIsland, EncounterSet.AbominableContessa]
        twice $ addChaosToken #tablet

      when (isNothing mstaff) do
        storyWithContinue' $ scope "theForgottenIsland" do
          p "body"
          ul do
            li "gatherSets"
            li "set"
            li.nested "locations" do
              li "seals"
              li "allosaurus"
            li "acts"
            li "shuffleRemaining"
            li "reelDeck"
            li "tokens"
            li "continue"

        jungleSet <- selectJust $ locationIs Locations.jungleSet
        eachInvestigator \iid -> moveTo attrs iid jungleSet
        selectEach (LocationWithTrait Set <> not_ (be jungleSet)) \other -> do
          removeLocation other
          setCardAside =<< field LocationCard other
        ruinsOfTheSerpentKing <- placeLocation Locations.ruinsOfTheSerpentKing
        jungleRiver <- placeLocation Locations.jungleRiver

        placeLocationCards
          [ Locations.westernRidge
          , Locations.tarPit
          , Locations.easternRidge
          ]
        push
          $ SetLayout
            [ ".         .            tarPit                ."
            , "jungleSet westernRidge ruinsOfTheSerpentKing easternRidge"
            , ".         .            jungleRiver           ."
            ]
        placeTokens attrs ruinsOfTheSerpentKing Seal 4
        createAssetAt_ Assets.staffOfTheSerpentRelicOfThePast (AtLocation ruinsOfTheSerpentKing)
        createEnemyAt_ Enemies.allosaurusIndomitablePredator jungleRiver
        setActDeck [Cards.andresRequest, Cards.theForgottenIsland, Cards.destroyTheSource]
        shuffleSetAsideEncounterSetIntoEncounterDeck EncounterSet.ForgottenIsland
        shuffleEncounterDiscardBackIn
        shuffleSetAsideIntoScenarioDeck ReelDeck
          $ SingleSidedCard
          <> fromSets [EncounterSet.CosmicJourney, EncounterSet.AbominableContessa]
        twice $ addChaosToken #elderthing

      when (isNothing mcape) do
        storyWithContinue' $ scope "theAbominableContessa" do
          p "body"
          ul do
            li "gatherSets"
            li "set"
            li.nested "locations" $ li "contessa"
            li "placeClues"
            li "acts"
            li "shuffleRemaining"
            li "reelDeck"
            li "tokens"
            li "continue"

        gothicSet <- selectJust $ locationIs Locations.gothicSet
        eachInvestigator \iid -> moveTo attrs iid gothicSet
        selectEach (LocationWithTrait Set <> not_ (be gothicSet)) \other -> do
          removeLocation other
          setCardAside =<< field LocationCard other

        throneOfBlood <- placeLocation Locations.throneOfBloodRedAsBloodBlackAsNight
        placeLocationCards
          [ Locations.castleHallwaysSeeminglyEndless
          , Locations.catacombsStinksOfDeath
          , Locations.clockTowerIncessantlyTicking
          , Locations.moonlitGardenPoisonedBeauty
          ]
        push
          $ SetLayout
            [ ".              throneOfBlood ."
            , "moonlitGarden  throneOfBlood clockTower"
            , "moonlitGarden  gothicSet     clockTower"
            , "castleHallways gothicSet     catacombs"
            , "castleHallways .             catacombs"
            ]
        createEnemyAt_ Enemies.theContessaNeedlesslySmug throneOfBlood
        n <- fieldMapM LocationRevealClues getGameValue gothicSet
        push $ PlaceCluesUpToClueValue gothicSet (toSource attrs) n
        setActDeck [Cards.andresRequest, Cards.theAbominableContessa, Cards.bloodbath]
        shuffleSetAsideEncounterSetIntoEncounterDeck EncounterSet.AbominableContessa
        shuffleEncounterDiscardBackIn
        shuffleSetAsideIntoScenarioDeck ReelDeck
          $ SingleSidedCard
          <> fromSets [EncounterSet.CosmicJourney, EncounterSet.ForgottenIsland]
        twice $ addChaosToken #cultist

      pure a
    _ -> AndresRequest <$> liftRunMessage msg attrs
