module Arkham.Homebrew.DarkMatter.Scenarios.InTheShadowOfEarth (inTheShadowOfEarth) where

import Arkham.Asset.Types (Field (AssetDamage))
import Arkham.Card
import Arkham.Helpers.Card (getVictoryPoints)
import Arkham.Helpers.Query (allInvestigators, getLead)
import Arkham.Helpers.Xp (XpBonus (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern EvidenceDeck)
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorDamage))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding (InvestigatorDamage)
import Arkham.Trait (Trait (Ally, Crew))

newtype InTheShadowOfEarth = InTheShadowOfEarth ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inTheShadowOfEarth :: Difficulty -> InTheShadowOfEarth
inTheShadowOfEarth difficulty = scenario InTheShadowOfEarth ":dark-matter:112" "In the Shadow of Earth" difficulty []

{- | Scenario reference card, ":dark-matter:112" \/ z-dark-matter-115
(docs\/homebrew\/data\/dark-matter-sets\/in_the_shadow_of_earth.md; the front
block is Easy \/ Standard, the OCR of the back image is Hard \/ Expert):

Easy \/ Standard
[skull]: -X. X is half the amount of damage on you and assets you control (rounded down).
[cultist]: -2. If you fail, deal 1 damage or 1 horror to an [[Ally]] asset you control.
[tablet]: Reveal another token. If you fail, take 1 damage and 1 horror.
[elder thing]: 0. You must either (choose one): Take 2 damage, or this test automatically fails.

Hard \/ Expert
[skull]: -X. X is half the amount of damage on you and assets you control (rounded up).
[cultist]: -3. If you fail, deal 1 damage or 1 horror to an [[Ally]] asset you control.
[tablet]: -1. Reveal another token. If you fail, take 1 damage and 1 horror.
[elder thing]: 0. You must either (choose one): Take 3 damage, or this test automatically fails.

Only the /values/ live here; the riders are the 'ResolveChaosToken' and
'FailedSkillTest' cases in 'RunMessage' below. [tablet] prints no number on Easy
\/ Standard, which is a modifier of 0, not "no modifier" — the same shape the
guide uses everywhere else.
-}
instance HasChaosTokenValue InTheShadowOfEarth where
  getChaosTokenValue iid tokenFace (InTheShadowOfEarth attrs) = case tokenFace of
    Skull -> do
      damage <- field InvestigatorDamage iid
      assetDamage <- selectSum AssetDamage (assetControlledBy iid)
      let total = damage + assetDamage
      -- Rounded down on easy/standard, rounded up on hard/expert.
      pure
        $ ChaosTokenValue Skull
        $ NegativeModifier
        $ byDifficulty attrs (total `div` 2) ((total + 1) `div` 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 0 1
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 0)
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "Put all remaining locations into play." Every [[Nostalgia II]] location
other than the Airlocks, which is placed separately as the starting location.
None of them has a scanning back, so all eight are on the table from setup.
-}
remainingLocations :: [CardDef]
remainingLocations =
  [ Locations.crewQuarters
  , Locations.engineRoomInTheShadowOfEarth
  , Locations.flightDeck
  , Locations.hydroponics
  , Locations.infirmaryInTheShadowOfEarth
  , Locations.shipMainframe
  , Locations.telecoms
  ]

instance RunMessage InTheShadowOfEarth where
  runMessage msg s@(InTheShadowOfEarth attrs) = runQueueT $ scenarioI18n "inTheShadowOfEarth" $ case msg of
    Setup -> runScenarioSetup InTheShadowOfEarth attrs do
      gather Set.InTheShadowOfEarth
      gather Set.DeepSpace
      setAgendaDeck
        [ Agendas.theNostalgiaII
        , Agendas.theThingFromEarth
        , Agendas.screamOfTheDead
        , Agendas.itsWeirdAndPissedOff
        ]
      setActDeck [Acts.isAnyoneHome, Acts.saveOurSouls, Acts.theShadowOfEarth]

      {- "Set aside the The Entity and The Feaster from Afar enemies." The Feaster
      has a scanning back, so it has to leave the gathered pool before the
      scanning deck is built — that step takes "all the *other* encounter cards
      with icons at the bottom of their back side". -}
      setAside [Enemies.theEntity, Enemies.theFeasterFromAfar]

      addScanningDeck

      {- "Create the facedown 'Evidence' deck. This is done by taking all the one
      sided story cards and shuffling them together." Every story card with a
      scanning back has already been pulled into the scanning deck, so what is
      left in the pool is exactly the one-sided ones. -}
      evidence <- shuffle =<< amongGathered (CardWithType StoryType)
      removeCards evidence

      {- "Without looking at it, put the top card of the 'Evidence' deck facedown
      under the scenario reference card. Repeat this process once for every 3
      tally marks under 'Impending Doom' in your Campaign Log." (5 Impending
      Doom => 2 cards in total.)

      These are marked facedown so the client renders card backs: resolution 1
      and act 2b both say to look at them without reading them, so their faces
      must not leak to the players before then. -}
      impendingDoom <- getImpendingDoom
      let (hidden, rest) = splitAt (1 + impendingDoom `div` 3) evidence
      push . PlaceUnderneath ScenarioTarget =<< traverse (setFacedown True) hidden

      addExtraDeck EvidenceDeck rest

      -- "Put all remaining locations into play. Each investigator begins play at
      -- the Airlocks."
      placeAll remainingLocations
      startAt =<< place Locations.airlocks
    -- [tablet]: "Reveal another token."
    ResolveChaosToken _ Tablet iid -> do
      drawAnotherChaosToken iid
      pure s
    {- [elder thing]: "0. You must either (choose one): Take 2 damage, or this
    test automatically fails." (3 damage on hard/expert.) Both options are
    always available — "you must either" is a mandatory choice, not a may. -}
    ResolveChaosToken _ ElderThing iid -> do
      let damage = byDifficulty attrs 2 3
      chooseOneM iid $ unscoped do
        countVar damage $ labeled' "takeDamage" $ assignDamage iid ElderThing damage
        labeled' "automaticallyFailTest" failSkillTest
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        {- [cultist]: "If you fail, deal 1 damage or 1 horror to an [[Ally]]
        asset you control." Controlled, not "at your location", and mandatory —
        but only if you control one. -}
        Cultist -> do
          allies <- select $ AssetWithTrait Ally <> assetControlledBy iid
          unless (null allies) do
            chooseTargetM iid allies \ally -> chooseOneM iid $ unscoped do
              countVar 1 $ labeled' "dealDamage" $ dealAssetDamage ally Cultist 1
              countVar 1 $ labeled' "dealHorror" $ dealAssetHorror ally Cultist 1
        -- [tablet]: "If you fail, take 1 damage and 1 horror."
        Tablet -> assignDamageAndHorror iid Tablet 1 1
        _ -> pure ()
      pure s
    {- Resolution 1: "For each of the story cards [under the scenario reference],
    reveal 1 random chaos token from the chaos bag." One token per card; the
    pairing is by position in the two lists. -}
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      continue_ iid
      hidden <- select $ UnderScenarioReferenceMatch $ CardWithType StoryType
      let
        imitations =
          [ crew
          | (card, token) <- zip hidden tokens
          , isImitationToken token.face
          , Just crew <- [crewForEvidence card]
          ]
      -- "If any of the imitations are in the victory display, proceed to (->R2).
      -- Otherwise (->R3)."
      anyImitationRescued <-
        if null imitations
          then pure False
          else selectAny $ VictoryDisplayCardMatch $ basic $ mapOneOf cardIs imitations
      push $ if anyImitationRescued then R2 else R3
      pure s
    ScenarioResolution res -> scope "resolutions" do
      case res of
        {- "If no resolution was reached (each investigator resigned or was
        defeated): If the current act is Act 3, proceed to Resolution 3.
        Otherwise, proceed to Resolution 1." -}
        NoResolution -> do
          onAct3 <- selectAny $ ActWithStep 3
          resolution "noResolution"
          push $ if onAct3 then R3 else R1
        Resolution 1 -> do
          resolution "resolution1"
          crewRescued <- selectAny $ VictoryDisplayCardMatch $ basic $ CardWithTrait Crew
          hidden <- select $ UnderScenarioReferenceMatch $ CardWithType StoryType
          if
            -- "If there are no [[Crew]] story assets in the victory display:
            -- Proceed directly to Resolution 5."
            | not crewRescued -> push R5
            -- Setup always hides at least one story card; with none there is
            -- nothing to unmask, so nobody rescued can be an imitation.
            | null hidden -> push R3
            | otherwise -> do
                lead <- getLead
                requestChaosTokens lead attrs (length hidden)
        Resolution 2 -> do
          resolution "resolution2"
          push R5
        Resolution 3 -> do
          record YouHaveRescuedTheRemainingCrewOfTheNostalgiaII
          {- "Add 1 tally mark under 'Impending Doom'. Then, add 1 additional
          tally mark for every 2 [[Crew]] story asset removed from the game,
          attached to the Entity, or in the scanning deck."

          'nub' (Eq Card is card-id equality) because the three zones overlap:
          act 2b searches for the crew that were removed from the game and
          attaches them to the Entity, and nothing takes those cards back out of
          the removed-from-game area. Each lost crew member must count once. -}
          lost <-
            nub
              . concat
              <$> sequence [getRemovedCrew, getCrewAttachedToTheEntity, getCrewInScanningDeck]
          addImpendingDoom $ 1 + (length lost `div` 2)
          resolutionWithXp "resolution3" $ allGainXp' attrs
          endOfScenario
        Resolution 4 -> do
          record TheNostalgiaIIHasBeenSaved
          {- "An investigator may add the Space Artillery story asset to their
          deck. This card does not count towards that investigator's deck
          limit." -}
          iids <- allInvestigators
          addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.spaceArtillery
          -- "Add 1 [cultist] token to the chaos bag for the remainder of the
          -- campaign for catching the eye of the King in Yellow."
          addChaosToken Cultist
          -- "Add 1 tally mark under 'Impending Doom'. Then, add 1 additional
          -- tally mark for every 2 [[Crew]] story asset attached to the Entity."
          attached <- getCrewAttachedToTheEntity
          addImpendingDoom $ 1 + (length attached `div` 2)
          {- "Each investigator earns experience equal to the Victory X value of
          each card in the victory display and of each [[Crew]] story asset that
          the investigators control." The victory display half is what
          'allGainXp' already computes; the surviving crew are still in play, so
          they are added as a bonus. -}
          crew <- select $ AssetWithTrait Crew <> AssetControlledBy Anyone
          rescued <- sum . catMaybes <$> traverse getVictoryPoints crew
          resolutionWithXp "resolution4"
            $ allGainXpWithBonus' attrs
            $ if rescued > 0 then WithBonus "Crew of the Nostalgia II rescued" rescued else NoBonus
          endOfScenario
        Resolution 5 -> do
          record YouCouldntSaveTheCrewOfTheNostalgiaII
          record AllInvestigatorsHaveBeenCorruptedByTheEarth
          addImpendingDoom 4
          -- "Each investigator earns 2 additional experience as they have seen
          -- the unthinkable."
          resolutionWithXp "resolution5"
            $ allGainXpWithBonus' attrs (WithBonus "They have seen the unthinkable" 2)
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> InTheShadowOfEarth <$> liftRunMessage msg attrs
