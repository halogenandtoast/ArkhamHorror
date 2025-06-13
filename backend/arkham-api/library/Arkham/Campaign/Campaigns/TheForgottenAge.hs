module Arkham.Campaign.Campaigns.TheForgottenAge (theForgottenAge, TheForgottenAge (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.CampaignSteps
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes.HasGame
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Investigator.Cards (leoAnderson, montereyJack, ursulaDowns)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (setActiveDuringSetup)
import Arkham.Projection
import Arkham.Question (Question (Read), ReadChoices (..))
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Aeson (Result (..))
import Data.Aeson.Types (parseMaybe)
import Data.Map qualified as Map
import Data.Monoid (Endo (..))

newtype TheForgottenAge = TheForgottenAge CampaignAttrs
  deriving newtype (Show, Eq, ToJSON, Entity, HasModifiersFor)

-- metadata is no longer used but we need games to update to deal with it
instance FromJSON TheForgottenAge where
  parseJSON = withObject "TheForgottenAge" $ \o -> do
    case parseMaybe parseJSON (Object o) of
      Just (metadata :: Metadata) -> TheForgottenAge . (\c -> c {campaignMeta = toJSON metadata}) <$> parseJSON (Object o)
      _ -> TheForgottenAge <$> parseJSON (Object o)

instance IsCampaign TheForgottenAge where
  nextStep a = case campaignStep (toAttrs a) of
    PrologueStep -> Just TheUntamedWilds
    TheUntamedWilds -> Just (InterludeStep 1 Nothing)
    InterludeStep 1 _ -> Just (UpgradeDeckStep TheDoomOfEztli)
    TheDoomOfEztli -> Just (UpgradeDeckStep $ InterludeStep 2 Nothing)
    InterludeStep 2 _ -> Just ThreadsOfFate
    ThreadsOfFate -> Just ResupplyPoint
    ResupplyPoint -> Just (UpgradeDeckStep TheBoundaryBeyond)
    TheBoundaryBeyond -> Just (UpgradeDeckStep $ InterludeStep 3 Nothing)
    InterludeStep 3 _ -> Just HeartOfTheElders
    HeartOfTheElders -> Just (UpgradeDeckStep TheCityOfArchives)
    TheCityOfArchives -> Just (UpgradeDeckStep $ InterludeStep 4 Nothing)
    InterludeStep 4 _ -> Just TheDepthsOfYoth
    TheDepthsOfYoth -> Just (UpgradeDeckStep $ InterludeStep 5 Nothing)
    InterludeStep 5 _ -> Just ShatteredAeons
    ShatteredAeons -> Nothing
    EpilogueStep -> Just (UpgradeDeckStep TurnBackTime)
    TurnBackTime -> Nothing
    UpgradeDeckStep nextStep' -> Just nextStep'
    _ -> Nothing

theForgottenAge :: Difficulty -> TheForgottenAge
theForgottenAge difficulty =
  campaign
    TheForgottenAge
    (CampaignId "04")
    "The Forgotten Age"
    difficulty
    (chaosBagContents difficulty)

initialSupplyPoints :: HasGame m => m Int
initialSupplyPoints = getPlayerCountValue (ByPlayerCount 10 7 5 4)

initialResupplyPoints :: HasGame m => m Int
initialResupplyPoints = getPlayerCountValue (ByPlayerCount 8 5 4 3)

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge attrs) = runQueueT $ campaignI18n do
    let metadata = toResultDefault mempty (campaignMeta attrs)
    case msg of
      CampaignStep PrologueStep -> scope "prologue" do
        flavor $ setTitle "earthIsNotOurs.title" >> p "earthIsNotOurs.body"
        totalSupplyPoints <- initialSupplyPoints
        expeditionLeaders <- select $ mapOneOf investigatorIs [ursulaDowns, leoAnderson, montereyJack]
        supplyMap <- mapFromList . map (,totalSupplyPoints) <$> allInvestigators
        let metadata' = Metadata supplyMap (yithians metadata) (expeditionLeader metadata) (bonusXp metadata)
        storyBuild do
          setTitle "title"
          p "body"
          ul $ li.validate (notNull expeditionLeaders) "expeditionLeader"
        when (notNull expeditionLeaders) do
          lead <- getLead
          chooseOneM lead do
            questionLabeled' "expeditionLeader"
            for_ expeditionLeaders \target -> cardLabeled target do
              push $ SetCampaignMeta $ toJSON (metadata' {expeditionLeader = Just target})
        eachInvestigator (`forInvestigator` msg)
        nextCampaignStep
        pure . TheForgottenAge $ attrs {campaignMeta = toJSON metadata'}
      ForInvestigator investigatorId (CampaignStep PrologueStep) -> do
        pickSupplies investigatorId metadata prologueSupplies msg
        pure c
      CampaignStep (InterludeStep 1 mkey) -> scope "interlude1" do
        investigators <- allInvestigators
        withBlanket <- getInvestigatorsWithSupply Blanket
        withoutBlanket <- getInvestigatorsWithoutSupply Blanket
        withMedicine <- flip concatMapM investigators \iid -> do
          n <- getSupplyCount iid Medicine
          pure $ replicate n iid
        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks) \(iid, Deck cards) -> do
              guard (any (`cardMatch` CardWithTitle "Poisoned") cards) $> iid
        provisions <-
          concatForM investigators \iid ->
            map (iid,) <$> fieldMap InvestigatorSupplies (filter (== Provisions)) iid
        investigatorsWithBinocularsPairs <- for investigators \iid -> do
          binoculars <- fieldMap InvestigatorSupplies (elem Binoculars) iid
          pure (iid, binoculars)

        storyOnlyBuild withBlanket do
          compose.green do
            p "blanket"
            p.valid "restfulSleep"
            p.invalid "tossingAndTurning"

        for_ withoutBlanket \iid -> do
          storyOnlyBuild [iid] do
            compose.green do
              p "blanket"
              p.invalid "restfulSleep"
              p.valid "tossingAndTurning"

          chooseOneM iid $ unscoped do
            questionLabeled' "chooseTrauma"
            questionLabeledCard iid
            countVar 1 $ labeled' "sufferPhysicalTrauma" $ sufferPhysicalTrauma iid 1
            countVar 1 $ labeled' "sufferMentalTrauma" $ sufferMentalTrauma iid 1

        let useProvisions = take (length investigators) provisions
        for_ useProvisions (uncurry useSupply)

        let lowOnRationsCount = length investigators - length provisions
        storyWithChooseNM' lowOnRationsCount (p.green "provisions") do
          for_ investigators \iid -> do
            cardLabeled iid do
              storyOnlyBuild [iid] $ p.green "lowOnRations"
              handleTarget iid CampaignSource iid

        storyWithChooseOneM' (p.green "lookout") do
          for_ investigatorsWithBinocularsPairs \(iid, hasBinoculars) -> do
            cardLabeled (unInvestigatorId iid) do
              if hasBinoculars
                then do
                  storyOnlyBuild [iid] $ p.green "shapesInTheTrees"
                  interludeXp iid (WithBonus "Gain further insight into the motivations of the Eztli." 2)
                else do
                  storyOnlyBuild [iid] $ p.green "eyesInTheDark"
                  sufferMentalTrauma iid 1

        when (notNull withMedicine && notNull withPoisoned) do
          storyWithChooseUpToNM'
            (min (length withMedicine) (length withPoisoned))
            "doNotUseMedicine"
            (p.green "medicine")
            do
              for_ (zip withPoisoned withMedicine) \(poisoned, doctor) -> do
                cardLabeled (unInvestigatorId poisoned) do
                  removeCampaignCardFromDeck poisoned Treacheries.poisoned
                  useSupply doctor Medicine

        push $ CampaignStep (InterludeStepPart 1 mkey 2)
        nextCampaignStep
        pure c
      CampaignStep (InterludeStepPart 1 _ 2) -> scope "interlude1" do
        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks)
              $ \(iid, Deck cards) -> guard (any (`cardMatch` CardWithTitle "Poisoned") cards) $> iid

        unless (null withPoisoned) do
          storyOnlyBuild withPoisoned (p.green "thePoisonSpreads")
          for_ withPoisoned (`sufferPhysicalTrauma` 1)
        pure c
      CampaignStep (InterludeStep 2 mkey) -> do
        recoveredTheRelicOfAges <- getHasRecord TheInvestigatorsRecoveredTheRelicOfAges
        let expeditionsEndStep = if recoveredTheRelicOfAges then 1 else 5
        push $ CampaignStep (InterludeStepPart 2 mkey expeditionsEndStep)
        pure c
      CampaignStep (InterludeStepPart 2 mkey 1) -> do
        story expeditionsEnd1
        lead <- getLead
        chooseOneM lead do
          labeled
            "It belongs in a museum. Alejandro and the museum staff will be able to study it and learn more about its purpose. - Proceed to Expedition’s End 2."
            $ push
            $ CampaignStep (InterludeStepPart 2 mkey 2)
          labeled
            "It is too dangerous to be on display. We should keep it hidden and safe until we know more about it. - Skip to Expedition's End 3."
            $ push
            $ CampaignStep (InterludeStepPart 2 mkey 3)
        pure c
      CampaignStep (InterludeStepPart 2 mkey 2) -> do
        story expeditionsEnd2
        record TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
        record TheInvestigatorsHaveEarnedAlejandrosTrust

        let inADeckAlready = any (any ((== Assets.alejandroVela) . toCardDef) . toList) attrs.storyCards

        unless inADeckAlready do
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn =<< fetchCard Assets.alejandroVela
        addChaosToken Tablet
        push $ CampaignStep (InterludeStepPart 2 mkey 4)
        pure c
      CampaignStep (InterludeStepPart 2 mkey 3) -> do
        story expeditionsEnd3
        record TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
        record AlejandroIsContinuingHisResearchOnHisOwn
        push $ CampaignStep (InterludeStepPart 2 mkey 4)
        pure c
      CampaignStep (InterludeStepPart 2 _ 4) -> do
        story expeditionsEnd4
        nextCampaignStep
        pure c
      CampaignStep (InterludeStepPart 2 _ 5) -> do
        story expeditionsEnd5
        nextCampaignStep
        pure c
      CampaignStep ResupplyPoint -> do
        eachInvestigator (`forInvestigator` msg)
        totalResupplyPoints <- initialResupplyPoints
        resupplyMap <- mapFromList . map (,totalResupplyPoints) <$> getInvestigators
        nextCampaignStep
        pure
          . TheForgottenAge
          $ attrs
            { campaignMeta =
                toJSON $ Metadata resupplyMap (yithians metadata) (expeditionLeader metadata) (bonusXp metadata)
            }
      ForInvestigator iid (CampaignStep ResupplyPoint) -> do
        let isReturnTo = attrs.id == "53"
        when isReturnTo $ doStep 1 msg -- convert xp to supply points
        doStep 2 msg -- remove poisoned
        doStep 3 msg -- heal trauma
        pickSupplies iid metadata resupplyPointSupplies msg
        pure c
      DoStep 0 (DoStep spend (ForInvestigator iid (CampaignStep ResupplyPoint))) -> do
        pure
          $ TheForgottenAge
          $ attrs
            { campaignMeta =
                toJSON
                  $ Metadata
                    (supplyPoints metadata)
                    (yithians metadata)
                    (expeditionLeader metadata)
                    ( Map.alter
                        (maybe Nothing (\v -> let v' = max 0 (v - spend) in guard (v' > 0) $> v'))
                        iid
                        (bonusXp metadata)
                    )
            }
      DoStep 1 (ForInvestigator iid (CampaignStep ResupplyPoint)) -> do
        let extraXp = Map.findWithDefault 0 iid (bonusXp metadata)
        xp <- field InvestigatorXp iid
        when (xp + extraXp >= 2) do
          chooseAmount' iid "supplyPointsToGain" "$supplyPoints" 0 (xp + extraXp `div` 2) CampaignTarget
        pure c
      ResolveAmounts iid (getChoiceAmount "$supplyPoints" -> n) CampaignTarget | n > 0 -> do
        let total = n * 2
        let extraXp = Map.findWithDefault 0 iid (bonusXp metadata)
        let remaining = max 0 (total - extraXp)
        doStep 0 (DoStep total (ForInvestigator iid (CampaignStep ResupplyPoint)))
        when (remaining > 0) $ push $ SpendXP iid remaining
        pure c
      DoStep 2 msg'@(ForInvestigator iid (CampaignStep ResupplyPoint)) -> do
        let extraXp = Map.findWithDefault 0 iid (bonusXp metadata)
        isPoisoned <- getIsPoisoned iid
        xp <- field InvestigatorXp iid
        let hasXp = xp + extraXp >= 3
        let toSpend = max 0 (3 - extraXp)

        when (isPoisoned && hasXp) do
          chooseOneM iid do
            questionLabeled "Visit St. Mary's?"
            questionLabeledCard iid
            labeled "Spend 3 xp to visit St. Mary's Hospital and remove a poisoned weakness" do
              doStep 0 (DoStep 3 msg') -- spend extra first
              push $ SpendXP iid toSpend
              removeCampaignCardFromDeck iid Treacheries.poisoned
            labeled "Do not remove poisoned weakness" nothing
        pure c
      DoStep 3 msg'@(ForInvestigator iid (CampaignStep ResupplyPoint)) -> do
        let extraXp = Map.findWithDefault 0 iid (bonusXp metadata)
        xp <- field InvestigatorXp iid
        hasPhysicalTrauma <- fieldP InvestigatorPhysicalTrauma (> 0) iid
        hasMentalTrauma <- fieldP InvestigatorMentalTrauma (> 0) iid
        let hasXp = xp + extraXp >= 5
        let canHealTrauma = (hasPhysicalTrauma || hasMentalTrauma) && hasXp
        let toSpend = max 0 (5 - extraXp)
        -- when is return to we can heal trauma any number of times
        let isReturnTo = attrs.id == "53"

        when canHealTrauma do
          chooseOneM iid do
            questionLabeled "Visit St. Mary's"
            questionLabeledCard iid
            when hasPhysicalTrauma do
              labeled "Spend 5 xp to visit St. Mary's Hospital and remove a physical trauma" do
                doStep 0 (DoStep 5 msg') -- spend extra first
                when (toSpend > 0) $ push $ SpendXP iid toSpend
                push $ HealTrauma iid 1 0
                when (isReturnTo && xp + extraXp - 5 >= 5) $ doStep 3 msg'
            when hasMentalTrauma do
              labeled "Spend 5 xp to visit St. Mary's Hospital and remove a mental trauma" do
                doStep 0 (DoStep 5 msg') -- spend extra first
                when (toSpend > 0) $ push $ SpendXP iid toSpend
                push $ HealTrauma iid 0 1
                when (isReturnTo && xp + extraXp - 5 >= 5) $ doStep 3 msg'
            labeled "Do not remove trauma" nothing

        pure c
      CampaignStep (InterludeStep 3 mkey) -> do
        investigators <- allInvestigators
        lead <- getLead

        story theJungleBeckons

        gasUpdate <-
          getInvestigatorsWithSupply Gasoline >>= \case
            [] -> do
              story outOfGas
              cannotMulligan <- toModifiers CampaignSource [CannotMulligan]
              pure $ ala Endo foldMap [modifiersL %~ insertWith (<>) iid cannotMulligan | iid <- investigators]
            x : _ -> do
              useSupply x Gasoline
              pure id

        getInvestigatorsWithSupply Map >>= \case
          [] -> pure ()
          xs -> do
            storyOnly xs aPathDiscovered
            record TheInvestigatorsMappedOutTheWayForward

        provisions <- concatForM investigators \iid -> do
          map (iid,) <$> fieldMap InvestigatorSupplies (filter (== Provisions)) iid
        for_ (take (length investigators) provisions) (uncurry useSupply)

        chooseNM lead (length investigators - length provisions) do
          questionLabeled
            "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
          for_ investigators \iid -> do
            cardLabeled (unInvestigatorId iid) do
              storyOnly [iid] lowOnRationsInterlude3
              handleTarget iid CampaignSource (InvestigatorTarget iid)

        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks) \(iid, Deck cards) -> do
              guard (any (`cardMatch` CardWithTitle "Poisoned") cards) $> iid

        withMedicine <- flip concatMapM investigators \iid -> do
          n <- getSupplyCount iid Medicine
          pure $ replicate n iid
        when (notNull withMedicine && notNull withPoisoned) do
          chooseUpToNM lead (min (length withMedicine) (length withPoisoned)) "Do not use medicine" do
            questionLabeled "Choose an investigator to remove Poisoned by using a medicine"
            for_ (zip withPoisoned withMedicine) \(poisoned, doctor) -> do
              cardLabeled (unInvestigatorId poisoned) do
                removeCampaignCardFromDeck poisoned Treacheries.poisoned
                useSupply doctor Medicine

        push $ CampaignStep (InterludeStepPart 3 mkey 2)

        canteenUpdate <-
          getInvestigatorsWithSupply Canteen >>= \case
            [] -> do
              story secretsInTheStone
              pure id
            xs -> do
              story patternsInTheStone
              startingClues <- toModifiers CampaignSource [StartingClues 1]
              pure $ ala Endo foldMap [modifiersL %~ insertWith (<>) iid startingClues | iid <- xs]

        isFaithRestored <-
          andM
            [ getHasRecord TheInvestigatorsForgedABondWithIchtaca
            , getHasRecord IchtacaHasConfidenceInYou
            , pure $ count (== Cultist) attrs.chaosBag >= 2
            ]
        when isFaithRestored do
          record IchtacasFaithIsRestored
          addChaosToken Cultist
        nextCampaignStep
        pure . TheForgottenAge $ attrs & gasUpdate & canteenUpdate
      CampaignStep (InterludeStepPart 3 _ 2) -> do
        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks)
              $ \(iid, Deck cards) ->
                if any (`cardMatch` CardWithTitle "Poisoned") cards
                  then Just iid
                  else Nothing

        storyOnly withPoisoned thePoisonSpreadsInterlude3
        for_ withPoisoned (`sufferPhysicalTrauma` 1)
        pure c
      CampaignStep (InterludeStep 4 mkey) -> do
        push $ CampaignStep (InterludeStepPart 4 mkey 1)
        push $ CampaignStep (InterludeStepPart 4 mkey 2)
        push $ CampaignStep (InterludeStepPart 4 mkey 3)
        push $ CampaignStep (InterludeStepPart 4 mkey 4)
        push $ CampaignStep (InterludeStepPart 4 mkey 5)
        push $ CampaignStep (InterludeStepPart 4 mkey 6)
        nextCampaignStep
        pure c
      CampaignStep (InterludeStepPart 4 _ 1) -> do
        backfired <- getHasRecord TheProcessBackfired
        backfiredSpectacularly <- getHasRecord TheProcessBackfiredSpectacularly
        -- no chaos bag technically so we sample from campaign
        if backfired || backfiredSpectacularly
          then do
            iids <- allInvestigators
            let chaosBag = fromJustNote "missing tokens" $ nonEmpty attrs.chaosBag
            results <- for iids $ \iid -> do
              tokens <- sampleN (if backfired then 1 else 2) chaosBag
              asChaosTokens <- traverse (\face -> createChaosToken face <&> revealedByL ?~ iid) tokens
              let
                outOfBody =
                  any
                    ( \t ->
                        t
                          `elem` [Cultist, Tablet, ElderThing, AutoFail, Skull]
                          || (t /= PlusOne && isNumberChaosToken t)
                    )
                    tokens
                stuckAsYithian = any (`elem` [Cultist, Tablet, ElderThing, AutoFail]) tokens
              pure (iid, outOfBody, stuckAsYithian, asChaosTokens)

            for_ results \(iid, outOfBody, stuckAsYithian, tokens) -> do
              let
                qLabel
                  | stuckAsYithian =
                      "You must use the Body of a Yithian investigator card as your investigator card for the remainder of the campaign. You also gain the Out of Body Experience weakness."
                  | outOfBody = "You gain the Out of Body Experience weakness"
                  | otherwise = "You suffer no ill-effects"
              focusChaosTokens tokens \unfocus -> do
                player <- getPlayer iid
                push $ Ask player $ Read (ft qLabel) (BasicReadChoices [Label "Continue" []]) Nothing
                push unfocus
              when outOfBody do
                addCampaignCardToDeck iid DoNotShuffleIn Treacheries.outOfBodyExperience

            let
              yithians =
                setFromList $ mapMaybe (\(iid, _, stuckAsYithian, _) -> guard stuckAsYithian $> iid) results

            pure
              . TheForgottenAge
              $ attrs
                { campaignMeta =
                    toJSON $ Metadata (supplyPoints metadata) yithians (expeditionLeader metadata) (bonusXp metadata)
                }
          else pure c
      CampaignStep (InterludeStepPart 4 mkey 2) -> do
        rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
        let
          allMet =
            and
              [ count (== Tablet) attrs.chaosBag == 2
              , rescuedAlejandro
              , mkey == Just TheCustodianWasUnderControl
              ]
        if allMet
          then do
            story aMindRecovered
            record AlejandroRemembersEverything
            addChaosToken Tablet
          else do
            story foreverLost
            record AlejandroIsSetAgainstYou
            removeCampaignCard Assets.alejandroVela
        pure c
      CampaignStep (InterludeStepPart 4 _ 3) -> do
        hasChalk <- getAnyHasSupply Chalk
        iids <- allInvestigators
        story $ if hasChalk then theWayIsOpen else theWayIsShut
        mods <- toModifiers CampaignSource [CannotMulligan]
        let
          update =
            if hasChalk
              then id
              else ala Endo foldMap $ [modifiersL %~ insertWith (<>) iid mods | iid <- iids]
        pure . TheForgottenAge $ attrs & update
      CampaignStep (InterludeStepPart 4 _ 4) -> do
        investigators <- allInvestigators
        provisions <- concatForM investigators \iid -> do
          map (iid,) <$> fieldMap InvestigatorSupplies (filter (== Provisions)) iid
        let
          lowOnRationsCount = length investigators - length provisions
          useProvisions = take (length investigators) provisions
        for_ useProvisions (uncurry useSupply)

        lead <- getLead
        chooseNM lead lowOnRationsCount do
          questionLabeled
            "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
          for_ investigators \iid -> do
            cardLabeled iid do
              storyOnly [iid] lowOnRationsInterlude4
              handleTarget iid CampaignSource iid
        pure c
      CampaignStep (InterludeStepPart 4 mkey 5) -> do
        investigators <- allInvestigators
        lead <- getLead
        withMedicine <- flip concatMapM investigators $ \iid -> do
          n <- getSupplyCount iid Medicine
          pure $ replicate n iid
        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks)
              $ \(iid, Deck cards) ->
                guard (any (`cardMatch` CardWithTitle "Poisoned") cards) $> iid
        when (notNull withMedicine && notNull withPoisoned) do
          chooseUpToNM lead (min (length withMedicine) (length withPoisoned)) "Do not use medicine" do
            questionLabeled "Choose an investigator to remove Poisoned by using a medicine"
            for_ (zip withPoisoned withMedicine) \(poisoned, doctor) -> do
              cardLabeled poisoned do
                removeCampaignCardFromDeck poisoned Treacheries.poisoned
                useSupply doctor Medicine
        push $ CampaignStep (InterludeStepPart 4 mkey 51)
        pure c
      CampaignStep (InterludeStepPart 4 _ 51) -> do
        let
          withPoisoned =
            flip mapMaybe (mapToList attrs.decks)
              $ \(iid, Deck cards) -> guard (any (`cardMatch` CardWithTitle "Poisoned") cards) $> iid
        storyOnly withPoisoned thePoisonSpreadsInterlude4
        for_ withPoisoned (`sufferPhysicalTrauma` 1)
        pure c
      CampaignStep (InterludeStepPart 4 _ 6) -> do
        withBlanket <- getInvestigatorsWithSupply Blanket
        withoutBlanket <- getInvestigatorsWithoutSupply Blanket
        storyOnly withBlanket restfulSleepInterlude4

        for_ withoutBlanket \iid -> do
          storyOnly [iid] tossingAndTurningInterlude4
          chooseOneM iid do
            questionLabeled "Choose trauma"
            questionLabeledCard iid
            labeled "Suffer physical trauma" $ sufferPhysicalTrauma iid 1
            labeled "Suffer mental trauma" $ sufferMentalTrauma iid 1
        pure c
      CampaignStep (InterludeStep 5 mkey) -> do
        whenHasRecord TheInvestigatorsFellIntoTheDepths $ story theDarkness1

        story theDarkness2
        push $ CampaignStep (InterludeStepPart 5 mkey 1)
        push $ CampaignStep (InterludeStepPart 5 mkey 2)
        push $ CampaignStep (InterludeStepPart 5 mkey 3)
        push $ CampaignStep (InterludeStepPart 5 mkey 4)
        nextCampaignStep

        pure c
      CampaignStep (InterludeStepPart 5 _ 1) -> do
        foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
        recoveredTheRelicOfAges <- getHasRecord TheInvestigatorsRecoveredTheRelicOfAges
        forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay
        mRelicOfAgesADeviceOfSomeSort <- getOwner Assets.relicOfAgesADeviceOfSomeSort
        mRelicOfAgesForestallingTheFutureOwner <- getOwner Assets.relicOfAgesForestallingTheFuture

        let
          mRelicOfAgesOwner = mRelicOfAgesADeviceOfSomeSort <|> mRelicOfAgesForestallingTheFutureOwner
          readFinalDawning = foundTheMissingRelic && recoveredTheRelicOfAges && forgingYourOwnPath
          newChaosToken = case attrs.difficulty of
            Easy -> MinusThree
            Standard -> MinusFour
            Hard -> MinusFive
            Expert -> MinusSix
        relic <- genCard Assets.relicOfAgesRepossessThePast

        if foundTheMissingRelic
          then story arcaneThrumming
          else do
            story growingConcern
            addChaosToken newChaosToken

        when readFinalDawning do
          story finalDawning
          removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort
          removeCampaignCard Assets.relicOfAgesForestallingTheFuture
          for_ mRelicOfAgesOwner \owner -> addCampaignCardToDeck owner DoNotShuffleIn relic
        pure c
      CampaignStep (InterludeStepPart 5 _ 2) -> do
        hasTorches <- getAnyHasSupply Torches
        story $ if hasTorches then torchlight else theAbyss
        record $ if hasTorches then TheBraziersAreLit else TheBraziersRemainUnlit
        pure c
      CampaignStep (InterludeStepPart 5 _ 3) -> do
        theBraziersAreLit <- getHasRecord TheBraziersAreLit
        hasMap <- select $ InvestigatorWithSupply Map
        when (theBraziersAreLit && notNull hasMap) $ do
          story readingSigns
          for_
            hasMap
            ( `interludeXp`
                WithBonus "Gains insight into the caverns that dwell beneath the surface of the Earth." 2
            )
        pure c
      CampaignStep (InterludeStepPart 5 _ 4) -> do
        eachInvestigator \iid -> do
          supplies <- field InvestigatorSupplies iid
          for_ supplies $ \case
            Medicine -> useSupply iid Medicine
            Provisions -> useSupply iid Provisions
            _ -> pure ()
        pure c
      CampaignStep EpilogueStep -> do
        -- We can only get here if we've turned back time, but may want to check
        setNextCampaignStep TurnBackTime
        pure c
      HandleTargetChoice _ CampaignSource (InvestigatorTarget iid) -> do
        mods <- map setActiveDuringSetup <$> toModifiers CampaignSource [StartingResources (-3)]
        pure . TheForgottenAge $ attrs & (modifiersL %~ insertWith (<>) iid mods)
      EndOfScenario _ -> do
        pure . TheForgottenAge $ attrs & modifiersL .~ mempty
      PickSupply investigatorId supply -> do
        let
          cost = supplyCost supply
          supplyMap = adjustMap (max 0 . subtract cost) investigatorId (supplyPoints metadata)
        pure
          . TheForgottenAge
          $ attrs
            { campaignMeta =
                toJSON $ Metadata supplyMap (yithians metadata) (expeditionLeader metadata) (bonusXp metadata)
            }
      PreScenarioSetup -> do
        pushAll $ map BecomeYithian $ toList $ yithians metadata
        pure c
      SetCampaignMeta value -> do
        case fromJSON value of
          Success meta' -> pure . TheForgottenAge $ attrs {campaignMeta = meta'}
          _ -> error "Invalid meta!"
      _ -> lift $ defaultCampaignRunner msg c
