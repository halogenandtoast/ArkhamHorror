module Arkham.Scenario.Scenarios.ReturnToTheMiskatonicMuseum (returnToTheMiskatonicMuseum) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheMiskatonicMuseum
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheMiskatonicMuseum = ReturnToTheMiskatonicMuseum TheMiskatonicMuseum
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheMiskatonicMuseum :: Difficulty -> ReturnToTheMiskatonicMuseum
returnToTheMiskatonicMuseum difficulty =
  scenario
    (ReturnToTheMiskatonicMuseum . TheMiskatonicMuseum)
    "51020"
    "Return to The Miskatonic Museum"
    difficulty
    scenarioLayout

instance RunMessage ReturnToTheMiskatonicMuseum where
  runMessage msg (ReturnToTheMiskatonicMuseum theMiskatonicMuseum'@(TheMiskatonicMuseum attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToTheMiskatonicMuseum . TheMiskatonicMuseum) attrs do
      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li.nested "exhibitDeck.instructions" do
            li "exhibitDeck.bottom"
            li "exhibitDeck.top"
          li "setAside"
          unscoped $ li "shuffleRemainder"
      scope "theVoid" $ flavor do
        setTitle "title"
        p "body"

      gather Set.ReturnToTheMiskatonicMuseum
      gather Set.TheMiskatonicMuseum
      gather Set.BadLuck
      gather Set.Sorcery
      gather Set.BeyondTheThreshold
      gather Set.CreepingCold
      gather Set.SecretDoors

      startAt =<< place Locations.museumEntrance

      securityOffice <- sample $ Locations.securityOffice_128 :| [Locations.securityOffice_129]
      administrationOffice <-
        sample $ Locations.administrationOffice_130 :| [Locations.administrationOffice_131]
      placeAll [Locations.museumHalls, securityOffice, administrationOffice]

      setAside
        [ Assets.haroldWalsted
        , Assets.adamLynch
        , Assets.theNecronomiconOlausWormiusTranslation
        , Treacheries.shadowSpawned
        ]

      setAgendaDeck [Agendas.restrictedAccess, Agendas.shadowsDeepen, Agendas.inEveryShadow]
      setActDeck
        [ Acts.findingAWayInside
        , Acts.nightAtTheMuseum
        , Acts.breakingAndEntering
        , Acts.searchingForTheTome
        ]

      (bottom, top) <-
        fmap (splitAt 2)
          . genCards
          . drop 2
          =<< shuffleM
            [ Locations.exhibitHallAthabaskanExhibit
            , Locations.exhibitHallMedusaExhibit
            , Locations.exhibitHallNatureExhibit
            , Locations.exhibitHallEgyptianExhibit
            , Locations.exhibitHallHallOfTheDead
            , Locations.exhibitHallMedievalExhibit
            , Locations.exhibitHallTheArchives
            ]
      restrictedHall <- genCard Locations.exhibitHallRestrictedHall
      bottom' <- shuffleM $ restrictedHall : bottom
      addExtraDeck ExhibitDeck $ top <> bottom'
      addAdditionalReferences ["51020b"]
    _ -> ReturnToTheMiskatonicMuseum <$> liftRunMessage msg theMiskatonicMuseum'
