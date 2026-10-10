{- | The Masque of the Red Death -- a fan side story.

Prospero's manor is a chain of seven coloured chambers hanging off the Grand
Ballroom, and every one of them charges clues or doom to enter. Act 1 asks the
whole party to walk that chain to its end; its advance turns the party into a
massacre -- The Red Death enters the Black Chamber, every guest flips to its
[[Victim]] face, and the Grand Ballroom's exit is locked again behind a fresh
clue toll.

Every location prints its own symbol and connections, so the grid below is
layout only -- the engine wires the map from the symbols.

Standalone has only two difficulties, Standard and Hard; the chaos bag is
chosen off 'isEasyStandard'.
-}
module Arkham.Homebrew.TheMasqueOfTheRedDeath.Scenarios.TheMasqueOfTheRedDeath (
  theMasqueOfTheRedDeath,
) where

import Arkham.Helpers.FlavorText
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Key
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Sets qualified as Set
import Arkham.Id (InvestigatorId)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Placement (Placement (AtLocation))
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted

newtype TheMasqueOfTheRedDeath = TheMasqueOfTheRedDeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMasqueOfTheRedDeath :: Difficulty -> TheMasqueOfTheRedDeath
theMasqueOfTheRedDeath difficulty =
  sideStory
    TheMasqueOfTheRedDeath
    ":the-masque-of-the-red-death:001"
    "The Masque of the Red Death"
    difficulty
    [ "whiteChamber  orangeChamber greenChamber"
    , "violetChamber grandBallroom purpleChamber"
    , "blackChamber  .             blueChamber"
    ]

instance HasChaosTokenValue TheMasqueOfTheRedDeath where
  getChaosTokenValue iid chaosTokenFace (TheMasqueOfTheRedDeath attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 0 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheMasqueOfTheRedDeath where
  runMessage msg s@(TheMasqueOfTheRedDeath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      scope "intro" $ flavor $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup TheMasqueOfTheRedDeath attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "prosperoPrince"
        li "dealGuests"
        li "setAside"
        unscoped $ li "shuffleRemainder"

      gather Set.TheMasqueOfTheRedDeath

      -- Every connection is printed, so nothing is wired here.
      grandBallroom <- placeLabeled "grandBallroom" Locations.grandBallroom
      startAt grandBallroom
      chambers <- for guestChambers (uncurry placeLabeled)
      placeLabeled_ "blackChamber" Locations.blackChamber

      placeAsset_ Assets.prosperoPrinceGregariousHost (AtLocation grandBallroom)

      {- "Randomly remove 2 Guest assets from the game. Shuffle the remaining
      Guest assets and put one into play at each Manor location except Grand
      Ballroom and Black Chamber." The two left over are never generated, which
      is what removing them from the game amounts to. -}
      dealt <- take (length chambers) <$> shuffleM guests
      for_ (zip chambers dealt) \(chamber, guest) -> assetAt_ guest chamber

      setAside [Enemies.theRedDeath, Assets.plaguePolyp]

      setAgendaDeck [Agendas.aNestOfVipers, Agendas.underTheSkin, Agendas.diseaseVectors]
      setActDeck [Acts.theSevenChambers, Acts.theMidnightHour]
    -- "[skull]: Add each [skull] effect on your location to this token."
    ResolveChaosToken token Skull iid -> do
      addSkullEffectsToToken iid token
      pure s
    -- "If you do not succeed by 2 or more": a pass whose margin is under the
    -- printed number counts too.
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      when (n < if isEasyStandard attrs then 2 else 3) $ symbolRider token.face iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      symbolRider token.face iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): If it was act 2 and at least 1 investigator resigned, read
        Resolution 2. Otherwise, read Resolution 1." -}
        NoResolution -> do
          onAct2 <- selectAny (ActWithStep 2)
          anyResigned <- selectAny $ IncludeEliminated ResignedInvestigator
          push $ ScenarioResolution $ Resolution $ if onAct2 && anyResigned then 2 else 1
        Resolution 1 -> do
          record TheRedDeathRavagedArkham
          resolution "resolution1"
          eachInvestigator (kill attrs)
          gameOver
        Resolution 2 -> do
          record TheRedDeathWasEnded
          resolutionWithXp "resolution2" $ allGainXp' attrs
          -- "An investigator may choose to add Plague Polyp to their deck. This
          -- card does not count toward that investigator's deck size."
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.plaguePolyp
          endOfScenario
        _ -> error $ "Unknown resolution: " <> show r
      pure s
    _ -> TheMasqueOfTheRedDeath <$> liftRunMessage msg attrs

{- | The rider the three symbol tokens share, once the test has come up short of
the margin they print.

[skull] has no rider of its own; its effects are whatever the chambers grant.
-}
symbolRider :: ReverseQueue m => ChaosTokenFace -> InvestigatorId -> m ()
symbolRider face iid = case face of
  Cultist -> chooseAndDiscardCard iid Cultist
  Tablet -> loseResources iid Tablet 1
  ElderThing -> chooseOneM iid $ withI18n $ countVar 1 do
    labeled "takeDamage" $ assignDamage iid ElderThing 1
    labeled "takeHorror" $ assignHorror iid ElderThing 1
  _ -> pure ()

-- | Standalone offers Standard and Hard only.
chaosBagContents :: Difficulty -> [ChaosTokenFace]
chaosBagContents = \case
  Hard -> hardBag
  Expert -> hardBag
  _ -> standardBag
 where
  symbols = [Skull, Skull, Skull, Cultist, Tablet, ElderThing, AutoFail, ElderSign]
  standardBag =
    [PlusOne, Zero, MinusOne, MinusOne, MinusTwo, MinusThree, MinusThree, MinusFour] <> symbols
  hardBag =
    [Zero, MinusOne, MinusTwo, MinusThree, MinusFour, MinusFour, MinusFive, MinusSix] <> symbols
