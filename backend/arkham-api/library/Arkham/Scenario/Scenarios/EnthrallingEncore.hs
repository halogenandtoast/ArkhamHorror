module Arkham.Scenario.Scenarios.EnthrallingEncore (enthrallingEncore) where

import Arkham.Id
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.EnthrallingEncore.Helpers

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour
  , Skull , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusThree , MinusFour
  , MinusFive , Skull , Skull , Skull , Cultist , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusThree , MinusFour , MinusFour
  , MinusFive , MinusSix , MinusEight , Skull , Skull , Skull , Cultist , Cultist , Tablet , ElderThing
  , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

newtype EnthrallingEncore = EnthrallingEncore ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enthrallingEncore :: Difficulty -> EnthrallingEncore
enthrallingEncore difficulty =
  sideStory
    EnthrallingEncore
    "90094"
    "Enthralling Encore"
    difficulty
    [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
    , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
    , "lobbyDoorway2 .     .       .         backstageDoorway2"
    ]

instance HasChaosTokenValue EnthrallingEncore where
  getChaosTokenValue iid tokenFace (EnthrallingEncore attrs) = case tokenFace of
    Skull -> do
      measures <- getMeasures
      pure $ toChaosTokenValue attrs Skull measures (measures + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> do
      multiclass <- (>= 3) <$> classesAmongControlledCards iid
      pure
        $ toChaosTokenValue attrs Tablet (if multiclass then 5 else 3) (if multiclass then 6 else 4)
    ElderThing -> do
      atSoloist <- selectAny $ enemyIs Enemies.sinisterSoloist <> enemyAtLocationWith iid
      if atSoloist && isHardExpert attrs
        then pure $ ChaosTokenValue ElderThing AutoFailModifier
        else pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage EnthrallingEncore where
  runMessage msg s@(EnthrallingEncore attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup EnthrallingEncore attrs do
      setup do
        ul do
          li "gatherSets"
          li "propsDeck"
          li "removeCards"
          li "setAside"
          li.nested "placeLocations" do
            li "startAt"
          li "sinisterSoloist"
          unscoped $ li "shuffleRemainder"

      gather Set.EnthrallingEncore
      gather Set.CurtainCall
      gather Set.DecayAndFilth
      gather Set.Delusions
      gather Set.Hauntings
      gather Set.Ghouls
      gather Set.StrikingFear
      gather Set.Rats

      setAgendaDeck [Agendas.theHauntingOfTheWardTheatre]
      setActDeck [Acts.breakFreeFromThePast]

      addExtraDeck PropsDeck =<< shuffle propsDeckCards

      removeEvery [Enemies.theManInThePallidMask, Enemies.royalEmissary]

      setAside
        [ Locations.lightingBox
        , Locations.boxOffice
        , Locations.greenRoom
        , Locations.dressingRoom
        , Locations.rehearsalRoom
        , Locations.trapRoom
        ]

      theatre <- place Locations.theatre
      backstage <- place Locations.backstage
      placeAll [Locations.lobby, Locations.balcony]
      startAt theatre
      enemyAt_ Enemies.sinisterSoloist backstage
    ResolveChaosToken _ ElderThing iid | isEasyStandard attrs -> do
      atSoloist <- selectAny $ enemyIs Enemies.sinisterSoloist <> enemyAtLocationWith iid
      when atSoloist $ push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | token.face == Cultist -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      if isEasyStandard attrs
        then chooseOrRunOneM iid do
          when hasClues $ labeled' "cultist.spendClue" $ spendClues iid 1
          labeled' "cultist.takeDamage" $ assignDamage iid Cultist 1
        else do
          when hasClues $ spendClues iid 1
          assignDamage iid Cultist 1
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          eachInvestigator \iid -> resolveSignatureSwap iid True
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          eachInvestigator \iid -> resolveSignatureSwap iid False
          endOfScenario
        _ -> error "Invalid resolution"
      pure s
    _ -> EnthrallingEncore <$> liftRunMessage msg attrs

resolveSignatureSwap :: (HasI18n, ReverseQueue m) => InvestigatorId -> Bool -> m ()
resolveSignatureSwap iid isOptional = do
  mswap <- getSignatureSwap iid
  owned <- getOwnedCardDefs iid
  let
    available f g = case mswap of
      Just sw | f sw `elem` owned -> Just (f sw, g sw)
      _ -> Nothing
    upgradeSignature = available (.signatureCard) (.advancedSignatureCard)
    downgradeWeakness = available (.advancedWeaknessCard) (.weaknessCard)
    upgradeWeakness = available (.weaknessCard) (.advancedWeaknessCard)
    downgradeSignature = available (.advancedSignatureCard) (.signatureCard)
    swapCard (old, new) = do
      removeCampaignCardFromDeck iid old
      addCampaignCardToDeck iid DoNotShuffleIn new
  if isOptional
    then case (upgradeSignature, downgradeWeakness) of
      (Nothing, Nothing) -> gainXp iid ScenarioSource (ikey "xp.unableToSwap") 2
      (mUpgrade, mDowngrade) -> chooseOneM iid do
        questionLabeled' "swapSignature"
        for_ mUpgrade \pair -> labeled' "upgradeSignature" $ swapCard pair
        for_ mDowngrade \pair -> labeled' "downgradeWeakness" $ swapCard pair
        labeled' "doNotSwap" nothing
    else case (upgradeWeakness, downgradeSignature) of
      (Nothing, Nothing) -> sufferMentalTrauma iid 1
      (mUpgrade, mDowngrade) -> chooseOrRunOneM iid do
        questionLabeled' "swapSignature"
        for_ mUpgrade \pair -> labeled' "upgradeWeakness" $ swapCard pair
        for_ mDowngrade \pair -> labeled' "downgradeSignature" $ swapCard pair
