module Arkham.Scenario.Scenarios.TheWagesOfSin (
  TheWagesOfSin (..),
  theWagesOfSin,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWagesOfSin.Story
import Arkham.Token
import Arkham.Trait (Trait (Spectral), toTraits)

newtype TheWagesOfSin = TheWagesOfSin ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWagesOfSin :: Difficulty -> TheWagesOfSin
theWagesOfSin difficulty =
  scenario
    TheWagesOfSin
    "05161"
    "The Wages of Sin"
    difficulty
    [ ".              theGallows    .             chapelAttic     ."
    , "hereticsGraves hauntedFields .             abandonedChapel chapelCrypt"
    , ".              .             hangmansBrook .               ."
    ]

instance HasTokenValue TheWagesOfSin where
  getTokenValue iid tokenFace (TheWagesOfSin attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
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

instance RunMessage TheWagesOfSin where
  runMessage msg s@(TheWagesOfSin attrs) = case msg of
    PreScenarioSetup -> do
      iids <- allInvestigatorIds
      push $ story iids intro
      pure s
    SetTokensForScenario -> do
      whenM getIsStandalone $ push (SetTokens standaloneTokens)
      pure s
    Setup -> do
      gatheredCards <-
        buildEncounterDeckExcluding
          [ Locations.abandonedChapel
          , Locations.abandonedChapelSpectral
          , Locations.hauntedFields
          , Locations.hauntedFieldsSpectral
          , Locations.hangmansBrook
          , Locations.hangmansBrookSpectral
          , Locations.chapelAttic_175
          , Locations.chapelAttic_176
          , Locations.chapelAtticSpectral_175
          , Locations.chapelAtticSpectral_176
          , Locations.theGallows_169
          , Locations.theGallows_170
          , Locations.theGallowsSpectral_169
          , Locations.theGallowsSpectral_170
          , Locations.hereticsGraves_171
          , Locations.hereticsGraves_172
          , Locations.hereticsGravesSpectral_171
          , Locations.hereticsGravesSpectral_172
          , Locations.chapelCrypt_173
          , Locations.chapelCrypt_174
          , Locations.chapelCryptSpectral_173
          , Locations.chapelCryptSpectral_174
          ]
          [ EncounterSet.TheWagesOfSin
          , EncounterSet.AnettesCoven
          , EncounterSet.CityOfSins
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.TrappedSpirits
          , EncounterSet.Witchcraft
          ]
      let (spectralEncounterDeck, encounterDeck) = partition (elem Spectral . toTraits) (unDeck gatheredCards)
      theWatcher <-
        map EncounterCard
          <$> gatherEncounterSet EncounterSet.TheWatcher
      spectralWebs <- traverse genCard (replicate 4 Assets.spectralWeb)
      heretics <-
        traverse genCard
          =<< sampleN
            4
            ( Enemies.heretic_A
                :| [ Enemies.heretic_C
                   , Enemies.heretic_E
                   , Enemies.heretic_G
                   , Enemies.heretic_I
                   , Enemies.heretic_K
                   ]
            )
      let setAsideCards = theWatcher <> spectralWebs <> heretics

      theGallows <-
        sample (Locations.theGallows_169 :| [Locations.theGallows_170])

      hereticsGraves <-
        sample (Locations.hereticsGraves_171 :| [Locations.hereticsGraves_172])

      chapelAttic <-
        sample (Locations.chapelAttic_175 :| [Locations.chapelAttic_176])

      chapelCrypt <-
        sample (Locations.chapelCrypt_173 :| [Locations.chapelCrypt_174])

      (hangmansBrookId, placeHangmansBrook) <- placeLocationCard Locations.hangmansBrook

      placements <-
        traverse
          placeLocationCard_
          [ theGallows
          , hereticsGraves
          , chapelAttic
          , chapelCrypt
          , Locations.hauntedFields
          , Locations.abandonedChapel
          ]

      pushAll $
        [SetEncounterDeck $ Deck encounterDeck]
          <> placements
          <> [placeHangmansBrook, MoveAllTo (toSource attrs) hangmansBrookId]

      agendas <- genCards [Agendas.theHangedManXII, Agendas.deathsApproach]
      acts <- genCards [Acts.inPursuitOfTheDead, Acts.inPursuitOfTheLiving]

      TheWagesOfSin
        <$> runMessage
          msg
          ( attrs
              & (decksL . at SpectralEncounterDeck ?~ map EncounterCard spectralEncounterDeck)
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
          )
    _ -> TheWagesOfSin <$> runMessage msg attrs
