module Arkham.Homebrew.CircusExMortis.Scenarios.OneNightOnly (oneNightOnly) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Card (toCardDef)
import Arkham.Card.CardDef (CardDef, cdCardCode)
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Enemies
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move (moveTo_, moveTowardsMatching)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Creature, Performer))

newtype OneNightOnly = OneNightOnly ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oneNightOnly :: Difficulty -> OneNightOnly
oneNightOnly difficulty =
  scenario OneNightOnly "z-circus-ex-mortis-001" "One Night Only" difficulty []

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "oneNightOnly" a

actOneVariants :: NonEmpty CardDef
actOneVariants =
  Acts.ratsInACage_005
    :| [ Acts.ratsInACage_006
       , Acts.ratsInACage_007
       , Acts.ratsInACage_008
       ]

behindTheCurtainMatcher :: InvestigatorMatcher
behindTheCurtainMatcher =
  oneOf [investigatorIs Investigators.dexterDrake, InvestigatorWithTrait Performer]

instance HasChaosTokenValue OneNightOnly where
  getChaosTokenValue iid tokenFace (OneNightOnly attrs) = case tokenFace of
    Skull -> do
      -- X is the number of Creature enemies in play (Hard/Expert: 1 + that).
      n <- selectCount $ EnemyWithTrait Creature
      let extra = if isHardExpert attrs then 1 else 0
      pure $ ChaosTokenValue Skull (NegativeModifier (extra + n))
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    ElderThing -> do
      -- Worse when testing against a Creature enemy.
      vsCreature <-
        getSkillTestTarget >>= \case
          Just (EnemyTarget eid) -> eid <=~> EnemyWithTrait Creature
          _ -> pure False
      pure
        $ toChaosTokenValue attrs ElderThing (if vsCreature then 3 else 1) (if vsCreature then 4 else 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage OneNightOnly where
  runMessage msg s@(OneNightOnly attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "body"
      behindTheCurtain <- select behindTheCurtainMatcher
      unless (null behindTheCurtain) do
        flavor $ setTitle "title" >> p "behindTheCurtain"
      pure s
    Setup -> runScenarioSetup OneNightOnly attrs do
      gather Set.OneNightOnly
      gather Set.IllusoryTricks
      gather Set.NewMoonDaredevils
      gather Set.NewMoonEntertainers
      gather Set.PanickedMasses
      gather Set.PrimordialEvils

      setAside
        [ Assets.illusoryLocus
        , Locations.circusGatesPathToFreedom
        , Enemies.disguisedMonstrosity
        ]

      firstRing <- place Locations.theBigTopFirstRing
      secondRing <- place Locations.theBigTopSecondRing
      thirdRing <- place Locations.theBigTopThirdRing
      placeAll
        [ Locations.carousel
        , Locations.gamesGallery
        , Locations.animalCages
        , Locations.performerTrailers
        ]

      monstrosity <- enemyAt Enemies.disguisedMonstrosity firstRing
      exhaustThis monstrosity

      actOne <- sample actOneVariants
      setActDeck [actOne, Acts.smokeAndMirrors, Acts.outAndAway]
      setAgendaDeck
        [ Agendas.theTrueFace
        , Agendas.houseOfHorrors
        , Agendas.mesmericMagic
        ]

      -- Each investigator chooses a seat (their starting Big Top ring).
      eachInvestigator \iid -> do
        chooseOneM iid $ scope "seats" do
          labeled' "firstRing" $ moveTo_ attrs iid firstRing
          labeled' "secondRing" $ moveTo_ attrs iid secondRing
          labeled' "thirdRing" $ moveTo_ attrs iid thirdRing
        behind <- iid <=~> behindTheCurtainMatcher
        when behind $ gainClues iid attrs 1
    FailedSkillTestWithToken iid Cultist -> do
      -- Move once toward the nearest The Big Top location.
      moveTowardsMatching attrs iid (NearestLocationToYou $ LocationWithTitle "The Big Top")
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          scope "resolution1" $ flavor $ setTitle "title" >> p "body"
          record TheRingmasterDoesNotSuspectYou
          -- Look at the back of the chosen act 1 and add the listed token.
          actStep <- getCurrentActStep
          when (actStep == 1) do
            mAct <- selectOne AnyAct
            for_ mAct \aid -> do
              cardCode <- fieldMap ActCard (cdCardCode . toCardDef) aid
              let token
                    | cardCode `elem` ["z-circus-ex-mortis-005", "z-circus-ex-mortis-006"] = Tablet
                    | otherwise = Cultist
              addChaosToken token
          push $ ScenarioResolution $ Resolution 3
        Resolution 2 -> do
          scope "resolution2" $ flavor $ setTitle "title" >> p "body"
          record TheRingmasterHasHisEyeOnYou
          push $ ScenarioResolution $ Resolution 3
        Resolution 3 -> do
          scope "resolution3" $ flavor $ setTitle "title" >> p "body"
          allGainXp attrs
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> OneNightOnly <$> liftRunMessage msg attrs
