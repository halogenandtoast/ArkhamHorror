module Arkham.Homebrew.CircusExMortis.Campaign (circusExMortis) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaign.Overlay
import Arkham.Card.CardCode (toCardCode, unCardCode)
import Arkham.Card.CardDef (CardDef)
import Arkham.Classes.HasGame (HasGame, getGame)
import Arkham.Decklist.Type (investigator_code)
import Arkham.Game.Base (gamePerformTarotReadings)
import Arkham.Helpers.Campaign (getOwner, stored)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWith, setActiveDuringSetup)
import Arkham.Helpers.Query (getInvestigators, getLeadPlayer)
import Arkham.Helpers.Xp (toBonus)
import Arkham.Homebrew.CircusExMortis.Achievements (runCircusExMortisAchievements)
import Arkham.Homebrew.CircusExMortis.CampaignSteps
import Arkham.Homebrew.CircusExMortis.CardDefs.Assets qualified as HBAssets
import Arkham.Homebrew.CircusExMortis.CardDefs.Skills qualified as Skills
import Arkham.Homebrew.CircusExMortis.ChaosBag
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.Homebrew.CircusExMortis.Key
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.I18n (Scope, investigatorNameVar, keyVar)
import Arkham.Investigator.Cards (allInvestigatorCards)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Question (DestinyDrawing (..), Question (PickDestiny))
import Arkham.Source
import Arkham.Target (Target (CampaignTarget))
import Arkham.Tarot (TarotCard (..), TarotCardArcana (..), TarotCardFacing (Upright))
import Arkham.Trait (Trait (Believer, Chosen, Clairvoyant, Miskatonic, Scholar))

{- | Swap a versioned campaign story card for its next version in the same
investigator's deck (Relic of Ages pattern: remove the old def, add the new one
without counting toward deck size). No-op when nobody owns the old version.
-}
swapCampaignCard :: ReverseQueue m => CardDef -> CardDef -> m ()
swapCampaignCard old new =
  getOwner old >>= traverse_ \iid -> do
    removeCampaignCard old
    addCampaignCardToDeck iid DoNotShuffleIn new

{- | "The investigators must decide (choose one)" between the two next versions of a
versioned story card -- the shape both Written in Stone and Good Omens use throughout.
Each option's locale key names both its label and the flavor that branch reads.
-}
chooseNextVersion
  :: (HasI18n, ReverseQueue m) => Scope -> CardDef -> [(Scope, CardDef)] -> m ()
chooseNextVersion question current options =
  storyWithChooseOneM (setTitle "title" >> p question) $ for_ options \(key, next) ->
    labeled key do
      flavor $ setTitle "title" >> p key
      swapCampaignCard current next

-- | The eight words the priestess of Diana names (guide p19).
destinyWords :: [Text]
destinyWords = ["heart", "pipes", "torch", "rock", "sigil", "stain", "prayer", "burden"]

{- | Deal a destiny to each investigator in turn: "each investigator must choose one of
the following options... each investigator must choose a different option" (guide p19).

@departed@ is 'getDepartedDestinies' -- the destinies of investigators who have left the
campaign. It is empty during Written in Stone, where nobody has left yet, and the question
is then exactly the printed one. A player joining afterwards is offered those destinies as
well, because the guide hands a destiny to whoever replaces its holder; @remaining@ never
contains a word a departed investigator still holds, so a word is claimed through its
holder or not at all.
-}
chooseDestinies
  :: (HasI18n, ReverseQueue m) => [Text] -> [(InvestigatorId, Text)] -> [InvestigatorId] -> m ()
chooseDestinies _ _ [] = pure ()
chooseDestinies remaining departed (iid : rest) =
  chooseOneM iid do
    questionLabeledCard iid
    questionLabeled $ if null departed then "destinyQuestion" else "destinyOrTransferQuestion"
    for_ departed \(oldIid, word) ->
      withInvestigatorName oldIid $ keyVar "destiny" word $ labeled "claimDestiny" do
        transferDestiny oldIid iid
        forfeitSeat oldIid
        chooseDestinies remaining (filter ((/= oldIid) . fst) departed) rest
    for_ remaining \word ->
      labeled word do
        recordDestiny iid word
        chooseDestinies (filter (/= word) remaining) departed rest

{- | 'investigatorNameVar' for an id whose investigator may no longer be at the table, so
a 'field' read would throw. The client resolves the live name from the card code; the
printed name is only the fallback.
-}
withInvestigatorName :: HasI18n => InvestigatorId -> (HasI18n => a) -> a
withInvestigatorName iid a = case lookup (toCardCode iid) allInvestigatorCards of
  Just def -> investigatorNameVar def a
  Nothing -> keyVar "iname" (unCardCode $ toCardCode iid) a

{- | "The investigator chosen to replace them": once a departed investigator's destiny has
been handed on, that investigator is out of the campaign for good.

Retiring only sets an investigator aside (@gameRetiredInvestigators@), from where the
continue screen can rejoin them, and nothing clears that map, so the forfeit is remembered
on the campaign instead and 'UnretireInvestigator' drops them. It has to be remembered
rather than inferred from "holds no destiny": an investigator who left before Written in
Stone never held one and must still be dealt one when they come back.
-}
forfeitSeat :: ReverseQueue m => InvestigatorId -> m ()
forfeitSeat iid =
  push $ InsertGlobal CampaignTarget "forfeitedSeats" (String . unCardCode $ toCardCode iid)

getForfeitedSeats :: HasGame m => m [Text]
getForfeitedSeats = fromMaybe [] <$> stored "forfeitedSeats"

newtype CircusExMortis = CircusExMortis CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

circusExMortis :: Difficulty -> CircusExMortis
circusExMortis = campaign CircusExMortis (CampaignId ":circus-ex-mortis") "Circus Ex Mortis"

instance IsCampaign CircusExMortis where
  campaignTokens = chaosBagContents

  -- \| Guide p14: Curse of the Rougarou is offered free immediately after Harm's
  --  Way. The side story itself is played with its own printed cards; All Points
  --  West is where its two reward cards are upgraded to their Circus printings.
  --
  campaignOverlays (CircusExMortis attrs) =
    [ CampaignOverlay
        { id = "circus-ex-mortis:rougarou"
        , name = "Circus Ex Mortis"
        , scenario = curseOfTheRougarouId
        , available = discounted
        , xpCost = if discounted then 0 else 1
        }
    ]
   where
    {- Completed steps are most-recent-first, but they also accumulate the
    bookkeeping steps the continue screen pushes (a ContinueCampaignStep is
    prepended on every answer), so the head is not the last scenario played.
    Read the scenarios only -- the same way 'playedCurseOfTheRougarouEnRoute'
    does -- or the discount is gone by the time the cost is charged. -}
    discounted = case mapMaybe (.scenario) attrs.completedSteps of
      (sid : _) -> ScenarioStep sid == HarmsWay
      _ -> False
  nextStep a = case (toAttrs a).normalizedStep of
    PrologueStep -> continue OneNightOnly
    OneNightOnly -> continue ThePrimrosePath
    ThePrimrosePath -> continue TheFutureAndThePast
    TheFutureAndThePast -> continue HarmsWay
    HarmsWay -> continue AllPointsWest
    AllPointsWest -> continue WrittenInStone
    WrittenInStone -> continue PiperAtTheGatesOfDawn
    PiperAtTheGatesOfDawn -> continue Bacchanalia
    Bacchanalia -> continue GoodOmens
    GoodOmens -> continue RedSunrise
    RedSunrise -> continue ThousandToOne
    ThousandToOne -> continue EpilogueStep
    EpilogueStep -> Nothing
    other -> defaultNextStep other

instance HasModifiersFor CircusExMortis where
  getModifiersFor (CircusExMortis attrs) = do
    -- Interlude "The Future and the Past", Lingua Franca (guide p10):
    -- Miskatonic, Scholar, or Believer investigators begin the next scenario
    -- (Harm's Way) with 1 additional card in their opening hand.
    when (attrs.normalizedStep == HarmsWay) do
      modifySelectWith
        CampaignSource
        (mapOneOf InvestigatorWithTrait [Miskatonic, Scholar, Believer])
        setActiveDuringSetup
        [StartingHand 1]

instance RunMessage CircusExMortis where
  runMessage msg c =
    runQueueT $ campaignI18n $ lift (runCircusExMortisAchievements msg) *> case msg of
    CampaignStep PrologueStep -> do
      scope "additionalRules" $ flavor $ setTitle "title" >> p "moonTokens"
      scope "prologue" do
        flavor do
          setTitle "title"
          p "body"
          ul $ li "addMoonTokens"
      replicateM_ 3 $ addChaosToken MoonToken
      whenM (gamePerformTarotReadings <$> getGame) $ scope "campaignReading" do
        leadPlayer <- getLeadPlayer
        storyWithChooseOneM (setTitle "title" >> p "body") do
          labeled "performCampaignReading" do
            push $ SetPerformTarotReadings False
            push
              $ Ask leadPlayer
              $ PickDestiny
              $ zipWith
                DestinyDrawing
                [ "oneNightOnly"
                , "thePrimrosePath"
                , "harm'sWay"
                , "allPointsWest"
                , "piperAtTheGatesOfDawn"
                , "bacchanalia"
                , "redSunrise"
                , "thousandToOne"
                ]
              $ map
                (TarotCard Upright)
                [ TheMagicianI
                , TheHermitIX
                , StrengthVIII
                , TheChariotVII
                , TheDevilXV
                , TemperanceXIV
                , TheSunXIX
                , TheMoonXVIII
                ]
          labeled "performIndividualReadings" nothing
      nextCampaignStep
      pure c
    -- Interlude: The Future and the Past (guide pp9-10)
    CampaignStep (InterludeStep 1 _) -> scope "theFutureAndThePast" do
      clairvoyants <- select $ InvestigatorWithTrait Clairvoyant
      for_ clairvoyants \iid -> interludeXp iid $ toBonus "farSeeing" 1
      flavor do
        setTitle "title"
        p "intro"
        -- Far-Seeing: 1 bonus xp for each Clairvoyant investigator; the guide
        -- restricts it to purchasing Augury cards (not enforced by the app).
        scope "farSeeing" $ p.green.validate (notNull clairvoyants) "body"
        p "thea"
        ul $ li "addAmaltheaWeaver"
      addCampaignCardToDeckChoice_ HBAssets.amaltheaWeaverCircusFortuneTeller

      linguists <- select $ mapOneOf InvestigatorWithTrait [Miskatonic, Scholar, Believer]
      flavor do
        setTitle "title"
        p "theTome"
        -- The +1 opening hand for the next scenario is applied via
        -- HasModifiersFor while the campaign step is Harm's Way.
        scope "linguaFranca" $ p.green.validate (notNull linguists) "body"
        p "apuleius"
        ul $ li "addDeCultusBestiae"
      addCampaignCardToDeckChoice_ HBAssets.deCultusBestiaeForgottenWorkOfApuleius

      addChaosToken MoonToken
      normans <- select $ InvestigatorWithTitle "Norman Withers"
      flavor do
        setTitle "title"
        p "eclipse"
        -- Like Clockwork grants Norman "up to 2 additional Seeker cards
        -- (level 1-2)" in deckbuilding; deck construction is external to the
        -- engine, so this is informational.
        scope "likeClockwork" $ p.green.validate (notNull normans) "body"
        p "escape"
        ul $ li "addMoonToken"

      nextCampaignStep
      pure c
    -- Interlude: Written in Stone (guide pp19-20)
    CampaignStep (InterludeStep 2 _) -> scope "writtenInStone" do
      flavor $ setTitle "title" >> p "intro"
      eachInvestigator \iid -> do
        chooseOneM iid do
          questionLabeledCard Skills.invocationOfDiana
          questionLabeled "addInvocationOfDianaQuestion"
          labeled "addInvocationOfDiana" $ addCampaignCardToDeck iid DoNotShuffleIn Skills.invocationOfDiana
          labeled "doNotAddInvocationOfDiana" nothing
      flavor $ setTitle "title" >> p "destinyIntro"
      investigators <- getInvestigators
      chooseDestinies destinyWords [] investigators
      chooseNextVersion
        "role"
        HBAssets.amaltheaWeaverCircusFortuneTeller
        [ ("determination", HBAssets.amaltheaWeaverAspirantOfCourage)
        , ("guidance", HBAssets.amaltheaWeaverAspirantOfWisdom)
        ]

      -- Further Reading, then No Choice's trauma heal for each Chosen investigator, then
      -- the Motive decision -- one screen each.
      chosen <- select $ InvestigatorWithTrait Chosen
      flavor do
        scope "furtherReading" do
          setTitle "title"
          p "body"
        scope "noChoice" $ p.green.validate (notNull chosen) "body"
      for_ chosen \iid -> do
        hasPhysical <- fieldP InvestigatorPhysicalTrauma (> 0) iid
        hasMental <- fieldP InvestigatorMentalTrauma (> 0) iid
        when (hasPhysical || hasMental) do
          chooseOneM iid do
            questionLabeledCard iid
            questionLabeled "healTraumaQuestion"
            when hasPhysical $ labeled "healPhysicalTrauma" $ push $ HealTrauma iid 1 0
            when hasMental $ labeled "healMentalTrauma" $ push $ HealTrauma iid 0 1
            labeled "doNotHealTrauma" nothing
      chooseNextVersion
        "motive"
        HBAssets.deCultusBestiaeForgottenWorkOfApuleius
        [ ("fanaticism", HBAssets.deCultusBestiaeInterpretationOfConviction)
        , ("nemesis", HBAssets.deCultusBestiaeInterpretationOfObsession)
        ]
      flavor $ setTitle "title" >> p "bookmark"
      addChaosToken MoonToken
      nextCampaignStep
      pure c
    -- Interlude: Good Omens (guide pp27-28)
    CampaignStep (InterludeStep 3 _) -> scope "goodOmens" do
      -- Which branch the interlude takes is decided by the Amalthea Weaver
      -- version in play, so the fork is shown validated rather than as prose.
      amalthea <- fmap snd <$> getAmaltheaWeaverOwner
      flavor do
        setTitle "title"
        ul $ li "destinyReminder"
        p "intro"
        ul $ li.nested "amaltheaCheck" do
          li.validate (amalthea == Just HBAssets.amaltheaWeaverAspirantOfCourage) "aspirantOfCourage"
          li.validate (amalthea == Just HBAssets.amaltheaWeaverAspirantOfWisdom) "aspirantOfWisdom"
      case amalthea of
        Just v
          | v == HBAssets.amaltheaWeaverAspirantOfCourage ->
              chooseNextVersion
                "moreToDo"
                v
                [ ("priorWarning", HBAssets.amaltheaWeaverOracleOfPurity)
                , ("sawItComing", HBAssets.amaltheaWeaverOracleOfResolve)
                ]
          | v == HBAssets.amaltheaWeaverAspirantOfWisdom ->
              chooseNextVersion
                "moreToSee"
                v
                [ ("writtenInInk", HBAssets.amaltheaWeaverOracleOfEnlightenment)
                , ("writtenInSmoke", HBAssets.amaltheaWeaverOracleOfMystery)
                ]
        _ -> pure ()
      deCultus <- fmap snd <$> getDeCultusBestiaeOwner
      scope "theLastWord" $ flavor do
        setTitle "title"
        p "body"
        ul $ li.nested "deCultusCheck" do
          li.validate
            (deCultus == Just HBAssets.deCultusBestiaeInterpretationOfConviction)
            "interpretationOfConviction"
          li.validate
            (deCultus == Just HBAssets.deCultusBestiaeInterpretationOfObsession)
            "interpretationOfObsession"
      case deCultus of
        Just v
          | v == HBAssets.deCultusBestiaeInterpretationOfConviction ->
              chooseNextVersion
                "theInfinite"
                v
                [ ("powersAbove", HBAssets.deCultusBestiaeProphecyOfTheBeyond)
                , ("powersBelow", HBAssets.deCultusBestiaeProphecyOfTheEternal)
                ]
          | v == HBAssets.deCultusBestiaeInterpretationOfObsession ->
              chooseNextVersion
                "theEndless"
                v
                [ ("againstTheFlood", HBAssets.deCultusBestiaeProphecyOfTheHorde)
                , ("againstTheStorm", HBAssets.deCultusBestiaeProphecyOfTheBehemoth)
                ]
        _ -> pure ()
      flavor $ setTitle "title" >> p "breakOfDawn"
      addChaosToken MoonToken
      nextCampaignStep
      pure c
    -- Epilogue (guide pp35-36); only reached when the investigators won.
    CampaignStep EpilogueStep -> scope "epilogue" do
      mTransformation <- getOwner Assets.monstrousTransformation
      mEsprit <- getOwner Assets.ladyEsprit
      flavor do
        setTitle "title"
        p "intro"
        p.green.validate (isJust mTransformation) "underWraps"
        p.green.validate (isJust mEsprit) "divergingPaths"
        p "thea"
      prophecyFulfilled <- getHasRecord TheProphecyWasFulfilled
      if prophecyFulfilled
        then do
          flavor $ setTitle "title" >> p "kernelOfTruth"
          interludeXpAll $ toBonus "kernelOfTruth" 2
        else flavor $ setTitle "title" >> p "grainOfSalt"
      flavor $ setTitle "title" >> p "lookingAhead"
      clashed <- getHasRecord TheInvestigatorsClashedWithBlake
      unmasked <- getHasRecord TheInvestigatorsUnmaskedBlake
      rallied <- getHasRecord TheCultRallies
      if (clashed || unmasked) && rallied
        then do
          flavor $ setTitle "title" >> p "encore"
          record TheNewMoonCircusMaySomedayReturn
          getAmaltheaWeaverOwner >>= traverse_ \(iid, _) ->
            addCampaignCardToDeck iid DoNotShuffleIn Assets.theTowerXVI
          getDeCultusBestiaeOwner >>= traverse_ \(iid, _) ->
            addCampaignCardToDeck iid DoNotShuffleIn Assets.theDevilXv
        else do
          flavor $ setTitle "title" >> p "restAssured"
          record TheNewMoonCircusWasNeverSeenAgain
      gameOver
      pure c
    -- Moon tokens, end of round (guide p1): for each moon token sealed on
    -- your investigator card, you must choose to keep it sealed or take 1
    -- damage or 1 horror and release it.
    EndRoundWindow -> scope "moonToken" do
      eachInvestigator \iid -> do
        tokens <- getSealedMoonTokens iid
        for_ tokens \token -> do
          chooseOneM iid do
            questionLabeledCard iid
            questionLabeled "endOfRoundQuestion"
            labeled "keepSealed" nothing
            labeled "takeDamageAndRelease" do
              assignDamage iid CampaignSource 1
              releaseMoonToken token
            labeled "takeHorrorAndRelease" do
              assignHorror iid CampaignSource 1
              releaseMoonToken token
      pure c
    {- Written in Stone's Destinies (guide p19): "If an investigator is killed or
    driven insane, their destiny is transferred to the investigator chosen to
    replace them." The entry is keyed by the departing investigator's id, so it is
    rewritten to the replacement's; 'ReplaceInvestigator' names the new decklist
    rather than an id, and its investigator code is that id. -}
    ReplaceInvestigator oldIid decklist -> do
      transferDestiny oldIid (investigator_code decklist)
      lift $ defaultCampaignRunner msg c
    {- The same transfer for a departure the guide does not name: a player can drop and
    another join mid-campaign, which is not the printed replacement. Thousand to One keys
    nearly every card off destinies, so an investigator at the table without one has
    nothing to play -- hence an invariant check rather than a hook on the join itself. A
    join arrives here through Campaign/Runner's @JoinCampaign@, whose 'chooseJoinDeck'
    continuation re-runs this step once the new seat's deck is loaded, and a rejoin through
    'UnretireInvestigator', which re-runs it too; checking here covers both, and covers
    them only once, because a seat that has been dealt a destiny no longer qualifies.

    'defaultCampaignRunner' pushes the continuation ask straight to the real queue while
    'runQueueT' flushes afterwards, so the destiny question lands in front of it. -}
    CampaignStep (ContinueCampaignStep _) -> do
      destinies <- getDestinies
      -- Empty until Written in Stone, where the step deals them itself.
      unless (null destinies) $ scope "writtenInStone" do
        -- NOT 'getInvestigators': it filters to @gamePlayerOrder@, which 'JoinCampaign'
        -- and 'LoadDecklist' never append to -- only @ForTarget GameTarget ResetGame@
        -- rebuilds it, at the next scenario's setup -- so a seat that has just joined is
        -- invisible to it right here, which is the whole case this clause exists for.
        seatsWithout <-
          filter (\iid -> isNothing $ lookup iid destinies) <$> select UneliminatedInvestigator
        departed <- getDepartedDestinies
        chooseDestinies
          (filter (`notElem` map snd destinies) destinyWords)
          departed
          seatsWithout
      lift $ defaultCampaignRunner msg c
    {- A seat whose destiny has been handed on cannot be used again (see 'forfeitSeat').
    Game/Runner has already put them back by the time this runs, so drop them again with
    the 'ForgetSeat' half, which does not set them aside a second time. Campaign/Runner's
    own 'UnretireInvestigator' clause does nothing but re-push the current step to hand the
    lead a fresh ask, and 'RemoveInvestigatorFromCampaign' re-pushes it as well, so it is
    skipped rather than leaving a second continuation ask queued behind the live one. -}
    UnretireInvestigator iid -> do
      forfeited <- getForfeitedSeats
      if unCardCode (toCardCode iid) `elem` forfeited
        then c <$ push (RemoveInvestigatorFromCampaign iid)
        else lift $ defaultCampaignRunner msg c
    _ -> lift $ defaultCampaignRunner msg c
