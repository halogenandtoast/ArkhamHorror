module Arkham.Campaign.Campaigns.TheForgottenAge (
  TheForgottenAge (..),
  theForgottenAge,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Runner
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Difficulty
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Campaign
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Treacheries
import Data.Monoid (Endo (..))

data Metadata = Metadata
  { supplyPoints :: Map InvestigatorId Int
  , yithians :: Set InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Metadata where
  Metadata a1 b1 <> Metadata a2 b2 = Metadata (a1 <> a2) (b1 <> b2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty

newtype TheForgottenAge = TheForgottenAge (CampaignAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

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
    (TheForgottenAge . (`with` mempty))
    (CampaignId "04")
    "The Forgotten Age"
    difficulty
    (chaosBagContents difficulty)

initialSupplyPoints :: HasGame m => m Int
initialSupplyPoints = getPlayerCountValue (ByPlayerCount 10 7 5 4)

initialResupplyPoints :: HasGame m => m Int
initialResupplyPoints = getPlayerCountValue (ByPlayerCount 8 5 4 3)

supplyCost :: Supply -> Int
supplyCost = \case
  Provisions -> 1
  Medicine -> 2
  Gasoline -> 1
  Rope -> 3
  Blanket -> 2
  Canteen -> 2
  Torches -> 3
  Compass -> 2
  Map -> 3
  Binoculars -> 2
  Chalk -> 2
  Pocketknife -> 2
  Pickaxe -> 2
  Pendant -> 1

supplyLabel :: Supply -> [Message] -> UI Message
supplyLabel s = case s of
  Provisions ->
    go
      "Provisions"
      "(1 supply point each): Food and water for one person. A must-have for any journey."
  Medicine ->
    go
      "Medicine"
      "(2 supply points each): To stave off disease, infection, or venom."
  Gasoline ->
    go "Gasoline" "(2 supply points each): Enough for a long journey by car."
  Rope ->
    go
      "Rope"
      "(3 supply points): Several long coils of strong rope.  Vital for climbing and spelunking."
  Blanket -> go "Blanket" "(2 supply points): For warmth at night."
  Canteen ->
    go "Canteen" "(2 supply points): Can be refilled at streams and rivers."
  Torches ->
    go
      "Torches"
      "(3 supply points): Can light up dark areas, or set sconces alight."
  Compass ->
    go
      "Compass"
      "(2 supply points): Can guide you when you are hopelessly lost."
  Map ->
    go
      "Map"
      "(3 supply points): Unmarked for now, but with time, you may be able to map out your surroundings."
  Binoculars ->
    go "Binoculars" "(2 supply points): To help you see faraway places."
  Chalk -> go "Chalk" "(2 supply points): For writing on rough stone surfaces."
  Pendant ->
    go
      "Pendant"
      "(1 supply point): Useless, but fond memories bring comfort to travelers far from home."
  Pocketknife ->
    go
      "Pocketknife"
      "(2 supply point): Too small to be used as a reliable weapon, but easily concealed."
  Pickaxe ->
    go "Pickaxe" "(2 supply point): For breaking apart rocky surfaces."
 where
  go label tooltip = TooltipLabel label (Tooltip tooltip)

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge (attrs `With` metadata)) = case msg of
    CampaignStep PrologueStep -> do
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      totalSupplyPoints <- initialSupplyPoints
      let supplyMap = mapFromList $ map (,totalSupplyPoints) investigatorIds
      pushAll
        $ [story players prologue]
        <> [ CampaignStep (InvestigatorCampaignStep iid PrologueStep)
           | iid <- investigatorIds
           ]
        <> [NextCampaignStep Nothing]
      pure
        . TheForgottenAge
        $ attrs
        `with` Metadata
          supplyMap
          (yithians metadata)
    CampaignStep (InvestigatorCampaignStep investigatorId PrologueStep) -> do
      let
        remaining = findWithDefault 0 investigatorId (supplyPoints metadata)
      investigatorSupplies <- field InvestigatorSupplies investigatorId
      player <- getPlayer investigatorId

      when (remaining > 0) $ do
        let
          availableSupply s = s `notElem` investigatorSupplies || s `elem` [Provisions, Medicine]
          affordableSupplies = filter ((<= remaining) . supplyCost) prologueSupplies
          availableSupplies = filter availableSupply affordableSupplies
        push
          $ Ask player
          $ PickSupplies remaining investigatorSupplies
          $ Label "Done" []
          : map
            ( \s ->
                supplyLabel
                  s
                  [ PickSupply investigatorId s
                  , CampaignStep $ InvestigatorCampaignStep investigatorId PrologueStep
                  ]
            )
            availableSupplies

      pure c
    CampaignStep (InterludeStep 1 mkey) -> do
      lead <- getLeadPlayer
      investigatorIds <- allInvestigatorIds
      investigatorPlayers <- allInvestigatorPlayers
      withBlanket <- traverse getPlayer =<< getInvestigatorsWithSupply Blanket
      withoutBlanket <- traverse (traverseToSnd getPlayer) =<< getInvestigatorsWithoutSupply Blanket
      withMedicine <- flip concatMapM investigatorIds $ \iid -> do
        n <- getSupplyCount iid Medicine
        pure $ replicate n iid
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing
      provisions <-
        concatForM investigatorIds \iid -> do
          provisions <- fieldMap InvestigatorSupplies (filter (== Provisions)) iid
          pure $ map (iid,) provisions
      investigatorsWithBinocularsPairs <- for investigatorIds $ \iid -> do
        binoculars <- fieldMap InvestigatorSupplies (elem Provisions) iid
        player <- getPlayer iid
        pure (iid, player, binoculars)
      let
        lowOnRationsCount = length investigatorIds - length provisions
        useProvisions = take (length investigatorIds) provisions
      pushAll
        $ [story withBlanket restfulSleep | notNull withBlanket]
        <> concatMap
          ( \(iid, player) ->
              [ story [player] tossingAndTurning
              , chooseOne
                  player
                  [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                  , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                  ]
              ]
          )
          withoutBlanket
        <> map (uncurry UseSupply) useProvisions
        <> [ questionLabel
            "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
            lead
            $ ChooseN
              lowOnRationsCount
              [ CardLabel
                (unInvestigatorId iid)
                [ story [player] lowOnRations
                , HandleTargetChoice
                    iid
                    CampaignSource
                    (InvestigatorTarget iid)
                ]
              | (iid, player) <- investigatorPlayers
              ]
           | lowOnRationsCount > 0
           ]
        <> [ questionLabel
              "The lead investigator must choose one investigator to be the group’s lookout. Then, that investigator checks his or her supplies. If he or she has binoculars, he or she reads Shapes in the Trees. Otherwise, he or she reads Eyes in the Dark."
              lead
              $ ChooseOne
                [ CardLabel
                  (unInvestigatorId iid)
                  ( if hasBinoculars
                      then [story [player] shapesInTheTrees, GainXP iid CampaignSource 2]
                      else [story [player] eyesInTheDark, SufferTrauma iid 0 1]
                  )
                | (iid, player, hasBinoculars) <- investigatorsWithBinocularsPairs
                ]
           ]
        <> [ questionLabel
            "Choose an investigator to removed poisoned by using a medicine"
            lead
            $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
            $ Done "Do not use medicine"
            : [ CardLabel
                (unInvestigatorId poisoned)
                [ RemoveCampaignCardFromDeck poisoned Treacheries.poisoned
                , UseSupply doctor Medicine
                ]
              | (poisoned, doctor) <- zip withPoisoned withMedicine
              ]
           | notNull withMedicine && notNull withPoisoned
           ]
        <> [CampaignStep (InterludeStepPart 1 mkey 2)]
        <> [NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 1 _ 2) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing

      withPoisonedPlayers <- traverse getPlayer withPoisoned

      pushAll
        $ if notNull withPoisoned
          then
            story withPoisonedPlayers thePoisonSpreads
              : [SufferTrauma iid 1 0 | iid <- withPoisoned]
          else []
      pure c
    CampaignStep (InterludeStep 2 mkey) -> do
      recoveredTheRelicOfAges <- getHasRecord TheInvestigatorsRecoveredTheRelicOfAges
      let expeditionsEndStep = if recoveredTheRelicOfAges then 1 else 5
      push $ CampaignStep (InterludeStepPart 2 mkey expeditionsEndStep)
      pure c
    CampaignStep (InterludeStepPart 2 mkey 1) -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ story players expeditionsEnd1
        , chooseOne
            lead
            [ Label
                "It belongs in a museum. Alejandro and the museum staff will be able to study it and learn more about its purpose. - Proceed to Expedition’s End 2."
                [CampaignStep (InterludeStepPart 2 mkey 2)]
            , Label
                "It is too dangerous to be on display. We should keep it hidden and safe until we know more about it. - Skip to Expedition's End 3."
                [CampaignStep (InterludeStepPart 2 mkey 3)]
            ]
        ]
      pure c
    CampaignStep (InterludeStepPart 2 mkey 2) -> do
      players <- allPlayers
      lead <- getLeadPlayer
      investigatorIds <- allInvestigatorIds
      let
        inADeckAlready =
          any ((== Assets.alejandroVela) . toCardDef)
            . concat
            . toList
            $ campaignStoryCards attrs
      pushAll
        $ [ story players expeditionsEnd2
          , Record TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
          , Record TheInvestigatorsHaveEarnedAlejandrosTrust
          ]
        <> [addCampaignCardToDeckChoice lead investigatorIds Assets.alejandroVela | not inADeckAlready]
        <> [AddChaosToken Tablet, CampaignStep (InterludeStepPart 2 mkey 4)]
      pure c
    CampaignStep (InterludeStepPart 2 mkey 3) -> do
      players <- allPlayers
      pushAll
        [ story players expeditionsEnd3
        , Record TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
        , Record AlejandroIsContinuingHisResearchOnHisOwn
        , CampaignStep (InterludeStepPart 2 mkey 4)
        ]
      pure c
    CampaignStep (InterludeStepPart 2 _ 4) -> do
      players <- allPlayers
      pushAll [story players expeditionsEnd4, NextCampaignStep Nothing]
      pure c
    CampaignStep (InterludeStepPart 2 _ 5) -> do
      players <- allPlayers
      pushAll [story players expeditionsEnd5, NextCampaignStep Nothing]
      pure c
    CampaignStep ResupplyPoint -> do
      investigatorIds <- allInvestigatorIds
      totalResupplyPoints <- initialResupplyPoints
      poisonedInvestigators <- filterM getIsPoisoned investigatorIds
      poisonedInvestigatorsWith3Xp <-
        traverse (traverseToSnd getPlayer) =<< filterM (fieldP InvestigatorXp (>= 3)) poisonedInvestigators

      investigatorsWhoCanHealTrauma <-
        catMaybes <$> for investigatorIds \iid -> do
          hasPhysicalTrauma <- fieldP InvestigatorPhysicalTrauma (> 0) iid
          hasMentalTrauma <- fieldP InvestigatorMentalTrauma (> 0) iid
          hasXp <- fieldP InvestigatorXp (>= 5) iid
          player <- getPlayer iid
          if (hasPhysicalTrauma || hasMentalTrauma) && hasXp
            then pure $ Just (iid, player, hasPhysicalTrauma, hasMentalTrauma)
            else pure Nothing

      let
        resupplyMap = mapFromList $ map (,totalResupplyPoints) investigatorIds

      pushAll
        $ [ chooseOne
            player
            [ Label
                "Spend 3 xp to visit St. Mary's Hospital and remove a poisoned weakness"
                [ SpendXP iid 3
                , RemoveCampaignCardFromDeck iid Treacheries.poisoned
                ]
            , Label "Do not remove poisoned weakness" []
            ]
          | (iid, player) <- poisonedInvestigatorsWith3Xp
          ]
        <> [ chooseOne player
            $ [ Label
                "Spend 5 xp to visit St. Mary's Hospital and remove a physical trauma"
                [SpendXP iid 5, HealTrauma iid 1 0]
              | hasPhysical
              ]
            <> [ Label
                "Spend 5 xp to visit St. Mary's Hospital and remove a physical trauma"
                [SpendXP iid 5, HealTrauma iid 0 1]
               | hasMental
               ]
            <> [Label "Do not remove trauma" []]
           | (iid, player, hasPhysical, hasMental) <- investigatorsWhoCanHealTrauma
           ]
        <> [ CampaignStep (InvestigatorCampaignStep iid ResupplyPoint)
           | iid <- investigatorIds
           ]
        <> [NextCampaignStep Nothing]
      pure
        . TheForgottenAge
        $ attrs
        `with` Metadata
          resupplyMap
          (yithians metadata)
    CampaignStep (InvestigatorCampaignStep investigatorId ResupplyPoint) -> do
      let remaining = findWithDefault 0 investigatorId (supplyPoints metadata)
      investigatorSupplies <- field InvestigatorSupplies investigatorId
      player <- getPlayer investigatorId

      when (remaining > 0) $ do
        let
          availableSupply s = s `notElem` investigatorSupplies || s `elem` [Provisions, Medicine, Gasoline]
          affordableSupplies = filter ((<= remaining) . supplyCost) resupplyPointSupplies
          availableSupplies = filter availableSupply affordableSupplies
        push
          $ Ask player
          $ PickSupplies remaining investigatorSupplies
          $ Label "Done" []
          : map
            ( \s ->
                supplyLabel
                  s
                  [ PickSupply investigatorId s
                  , CampaignStep $ InvestigatorCampaignStep investigatorId ResupplyPoint
                  ]
            )
            availableSupplies

      pure c
    CampaignStep (InterludeStep 3 mkey) -> do
      investigatorPlayers <- allInvestigatorPlayers
      let iids = map fst investigatorPlayers
      let players = map snd investigatorPlayers

      lead <- getLeadPlayer
      withGasoline <- getInvestigatorsWithSupply Gasoline
      withCanteen <- getInvestigatorsWithSupply Canteen
      withMap <- traverse getPlayer =<< getInvestigatorsWithSupply Map
      isFaithRestored <-
        andM
          [ getHasRecord TheInvestigatorsForgedABondWithIchtaca
          , getHasRecord IchtacaHasConfidenceInYou
          , pure $ count (== Cultist) (campaignChaosBag attrs) >= 2
          ]
      provisions <-
        concatForM iids \iid -> do
          provisions <- fieldMap InvestigatorSupplies (filter (== Provisions)) iid
          pure $ map (iid,) provisions
      withMedicine <- flip concatMapM iids $ \iid -> do
        n <- getSupplyCount iid Medicine
        pure $ replicate n iid
      let
        (gasMessages, gasUpdate) = case withGasoline of
          [] ->
            ( [story players outOfGas]
            , ala Endo foldMap
                $ [ modifiersL %~ insertWith (<>) iid [toModifier CampaignSource CannotMulligan]
                  | iid <- iids
                  ]
            )
          x : _ -> ([UseSupply x Gasoline], id)
        mapMessages = case withMap of
          [] -> []
          xs ->
            [ story xs aPathDiscovered
            , Record TheInvestigatorsMappedOutTheWayForward
            ]
        (canteenMessages, canteenUpdate) = case withCanteen of
          [] -> ([story players secretsInTheStone], id)
          xs ->
            ( [story players patternsInTheStone]
            , ala
                Endo
                foldMap
                [ modifiersL %~ insertWith (<>) iid [toModifier CampaignSource $ StartingClues 1]
                | iid <- xs
                ]
            )
        lowOnRationsCount = length iids - length provisions
        useProvisions = take (length iids) provisions
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing
      pushAll
        $ story players theJungleBeckons
        : gasMessages
          <> mapMessages
          <> map (uncurry UseSupply) useProvisions
          <> [ questionLabel
              "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
              lead
              $ ChooseN
                lowOnRationsCount
                [ CardLabel
                  (unInvestigatorId iid)
                  [ story [player] lowOnRationsInterlude3
                  , HandleTargetChoice iid CampaignSource (InvestigatorTarget iid)
                  ]
                | (iid, player) <- investigatorPlayers
                ]
             | lowOnRationsCount > 0
             ]
          <> [ questionLabel
              "Choose an investigator to removed poisoned by using a medicine"
              lead
              $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
              $ Done "Do not use medicine"
              : [ CardLabel
                  (unInvestigatorId poisoned)
                  [ RemoveCampaignCardFromDeck poisoned Treacheries.poisoned
                  , UseSupply doctor Medicine
                  ]
                | (poisoned, doctor) <- zip withPoisoned withMedicine
                ]
             | notNull withMedicine && notNull withPoisoned
             ]
          <> [CampaignStep (InterludeStepPart 3 mkey 2)]
          <> canteenMessages
          <> ( guard isFaithRestored
                *> [Record IchtacasFaithIsRestored, AddChaosToken Cultist]
             )
          <> [NextCampaignStep Nothing]
      pure
        . TheForgottenAge
        . (`with` metadata)
        $ attrs
        & gasUpdate
        & canteenUpdate
    CampaignStep (InterludeStepPart 3 _ 2) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing
      withPoisonedPlayers <- traverse getPlayer withPoisoned
      pushAll
        $ guard (notNull withPoisoned)
        *> story withPoisonedPlayers thePoisonSpreadsInterlude3
        : [SufferTrauma iid 1 0 | iid <- withPoisoned]
      pure c
    CampaignStep (InterludeStep 4 mkey) -> do
      pushAll
        [ CampaignStep (InterludeStepPart 4 mkey 1)
        , CampaignStep (InterludeStepPart 4 mkey 2)
        , CampaignStep (InterludeStepPart 4 mkey 3)
        , CampaignStep (InterludeStepPart 4 mkey 4)
        , CampaignStep (InterludeStepPart 4 mkey 5)
        , CampaignStep (InterludeStepPart 4 mkey 6)
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (InterludeStepPart 4 _ 1) -> do
      backfired <- getHasRecord TheProcessBackfired
      backfiredSpectacularly <- getHasRecord TheProcessBackfiredSpectacularly
      iids <- allInvestigatorIds
      -- no chaos bag technically so we sample from campaign
      let
        chaosBag =
          fromJustNote "missing tokens" $ nonEmpty $ campaignChaosBag attrs
      if backfired || backfiredSpectacularly
        then do
          results <- for iids $ \iid -> do
            tokens <- sampleN (if backfired then 1 else 2) chaosBag
            asChaosTokens <-
              traverse (\face -> ChaosToken <$> getRandom <*> pure face <*> pure (Just iid)) tokens
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
            player <- getPlayer iid
            pure (iid, player, outOfBody, stuckAsYithian, asChaosTokens)

          let
            yithians =
              setFromList
                $ mapMaybe
                  ( \(iid, _, _, stuckAsYithian, _) ->
                      if stuckAsYithian then Just iid else Nothing
                  )
                  results

          pushAll
            $ concatMap
              ( \(iid, player, outOfBody, stuckAsYithian, tokens) ->
                  let
                    qLabel
                      | stuckAsYithian =
                          "You must use the Body of a Yithian investigator card as your investigator card for the remainder of the campaign. You also gain the Out of Body Experience weakness."
                      | outOfBody = "You gain the Out of Body Experience weakness"
                      | otherwise = "You suffer no ill-effects"
                   in
                    [ FocusChaosTokens tokens
                    , Ask player $ Read qLabel [Label "Continue" []]
                    , UnfocusChaosTokens
                    ]
                      <> [ AddCampaignCardToDeck iid Treacheries.outOfBodyExperience
                         | outOfBody
                         ]
              )
              results
          pure
            . TheForgottenAge
            $ attrs
            `With` Metadata (supplyPoints metadata) yithians
        else pure c
    CampaignStep (InterludeStepPart 4 mkey 2) -> do
      players <- allPlayers
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      let
        allMet =
          and
            [ count (== Tablet) (campaignChaosBag attrs) == 2
            , rescuedAlejandro
            , mkey == Just TheCustodianWasUnderControl
            ]
      pushAll
        $ if allMet
          then
            [ story players aMindRecovered
            , Record AlejandroRemembersEverything
            , AddChaosToken Tablet
            ]
          else
            [ story players foreverLost
            , Record AlejandroIsSetAgainstYou
            , RemoveCampaignCard Assets.alejandroVela
            ]
      pure c
    CampaignStep (InterludeStepPart 4 _ 3) -> do
      hasChalk <- getAnyHasSupply Chalk
      iids <- allInvestigatorIds
      players <- allPlayers
      let storyEntry = if hasChalk then theWayIsOpen else theWayIsShut
      push $ story players storyEntry

      let
        update =
          if hasChalk
            then id
            else
              ala Endo foldMap
                $ [ modifiersL %~ insertWith (<>) iid [toModifier CampaignSource CannotMulligan]
                  | iid <- iids
                  ]
      pure . TheForgottenAge . (`with` metadata) $ attrs & update
    CampaignStep (InterludeStepPart 4 _ 4) -> do
      investigatorIds <- allInvestigatorIds
      lead <- getLeadPlayer
      provisions <-
        concat <$> for investigatorIds \iid -> do
          provisions <-
            fieldMap
              InvestigatorSupplies
              (filter (== Provisions))
              iid
          pure $ map (iid,) provisions
      let
        lowOnRationsCount = length investigatorIds - length provisions
        useProvisions = take (length investigatorIds) provisions
      investigatorPlayers <- traverse (traverseToSnd getPlayer) investigatorIds
      pushAll
        $ map (uncurry UseSupply) useProvisions
        <> [ questionLabel
            "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
            lead
            $ ChooseN
              lowOnRationsCount
              [ CardLabel
                (unInvestigatorId iid)
                [ story [player] lowOnRationsInterlude4
                , HandleTargetChoice iid CampaignSource (InvestigatorTarget iid)
                ]
              | (iid, player) <- investigatorPlayers
              ]
           | lowOnRationsCount > 0
           ]
      pure c
    CampaignStep (InterludeStepPart 4 mkey 5) -> do
      investigatorIds <- allInvestigatorIds
      lead <- getLeadPlayer
      withMedicine <- flip concatMapM investigatorIds $ \iid -> do
        n <- getSupplyCount iid Medicine
        pure $ replicate n iid
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing
      pushAll
        $ [ questionLabel
            "Choose an investigator to removed poisoned by using a medicine"
            lead
            $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
            $ Done "Do not use medicine"
            : [ CardLabel
                (unInvestigatorId poisoned)
                [ RemoveCampaignCardFromDeck poisoned Treacheries.poisoned
                , UseSupply doctor Medicine
                ]
              | (poisoned, doctor) <- zip withPoisoned withMedicine
              ]
          | notNull withMedicine && notNull withPoisoned
          ]
        <> [CampaignStep (InterludeStepPart 4 mkey 51)]
      pure c
    CampaignStep (InterludeStepPart 4 _ 51) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
              if any (`cardMatch` CardWithTitle "Poisoned") cards
                then Just iid
                else Nothing
      withPoisonedPlayers <- traverse getPlayer withPoisoned
      pushAll
        $ if notNull withPoisoned
          then
            story withPoisonedPlayers thePoisonSpreadsInterlude4
              : [SufferTrauma iid 1 0 | iid <- withPoisoned]
          else []
      pure c
    CampaignStep (InterludeStepPart 4 _ 6) -> do
      withBlanket <- traverse getPlayer =<< getInvestigatorsWithSupply Blanket
      withoutBlanket <- getInvestigatorsWithoutSupply Blanket
      withoutBlanketPlayers <- traverse (traverseToSnd getPlayer) withoutBlanket
      pushAll
        $ [story withBlanket restfulSleepInterlude4 | notNull withBlanket]
        <> concatMap
          ( \(iid, player) ->
              [ story [player] tossingAndTurningInterlude4
              , chooseOne
                  player
                  [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                  , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                  ]
              ]
          )
          withoutBlanketPlayers
      pure c
    CampaignStep (InterludeStep 5 mkey) -> do
      players <- allPlayers
      fellIntoTheDepths <- getHasRecord TheInvestigatorsFellIntoTheDepths
      pushAll
        $ [story players theDarkness1 | fellIntoTheDepths]
        <> [ story players theDarkness2
           , CampaignStep (InterludeStepPart 5 mkey 1)
           , CampaignStep (InterludeStepPart 5 mkey 2)
           , CampaignStep (InterludeStepPart 5 mkey 3)
           , CampaignStep (InterludeStepPart 5 mkey 4)
           , NextCampaignStep Nothing
           ]
      pure c
    CampaignStep (InterludeStepPart 5 _ 1) -> do
      players <- allPlayers
      foundTheMissingRelic <- getHasRecord TheInvestigatorsFoundTheMissingRelic
      recoveredTheRelicOfAges <-
        getHasRecord
          TheInvestigatorsRecoveredTheRelicOfAges
      forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay

      mRelicOfAgesADeviceOfSomeSort <-
        getOwner
          Assets.relicOfAgesADeviceOfSomeSort
      mRelicOfAgesForestallingTheFutureOwner <-
        getOwner
          Assets.relicOfAgesForestallingTheFuture

      let
        mRelicOfAgesOwner = mRelicOfAgesADeviceOfSomeSort <|> mRelicOfAgesForestallingTheFutureOwner
        readFinalDawning = foundTheMissingRelic && recoveredTheRelicOfAges && forgingYourOwnPath
        newChaosToken = case campaignDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFour
          Hard -> MinusFive
          Expert -> MinusSix
      pushAll
        $ [story players arcaneThrumming | foundTheMissingRelic]
        <> [story players growingConcern | not foundTheMissingRelic]
        <> [AddChaosToken newChaosToken | not foundTheMissingRelic]
        <> [story players finalDawning | readFinalDawning]
        <> [ RemoveCampaignCard Assets.relicOfAgesADeviceOfSomeSort
           | readFinalDawning
           ]
        <> [ RemoveCampaignCard Assets.relicOfAgesForestallingTheFuture
           | readFinalDawning
           ]
        <> [ AddCampaignCardToDeck iid Assets.relicOfAgesRepossessThePast
           | readFinalDawning
           , iid <- maybeToList mRelicOfAgesOwner
           ]
      pure c
    CampaignStep (InterludeStepPart 5 _ 2) -> do
      players <- allPlayers
      hasTorches <- getAnyHasSupply Torches
      pushAll
        $ [ story players $ if hasTorches then torchlight else theAbyss
          , Record
              $ if hasTorches then TheBraziersAreLit else TheBraziersRemainUnlit
          ]
      pure c
    CampaignStep (InterludeStepPart 5 _ 3) -> do
      theBraziersAreLit <- getHasRecord TheBraziersAreLit
      hasMap <- select $ InvestigatorWithSupply Map
      when (theBraziersAreLit && notNull hasMap) $ do
        players <- allPlayers
        pushAll $ story players readingSigns : map (\iid -> GainXP iid CampaignSource 2) hasMap
      pure c
    CampaignStep (InterludeStepPart 5 _ 4) -> do
      iids <- allInvestigatorIds
      for_ iids $ \iid -> do
        supplies <- field InvestigatorSupplies iid
        for_ supplies $ \case
          Medicine -> push $ UseSupply iid Medicine
          Provisions -> push $ UseSupply iid Provisions
          _ -> pure ()
      pure c
    CampaignStep EpilogueStep -> do
      -- We can only get here if we've turned back time, but may want to check
      push $ NextCampaignStep (Just $ ScenarioStep "04344")
      pure c
    HandleTargetChoice _ CampaignSource (InvestigatorTarget iid) -> do
      pure
        . TheForgottenAge
        . (`with` metadata)
        $ attrs
        & ( modifiersL
              %~ insertWith
                (<>)
                iid
                [toModifier CampaignSource $ StartingResources (-3)]
          )
    EndOfScenario _ -> do
      pure . TheForgottenAge . (`with` metadata) $ attrs & modifiersL .~ mempty
    PickSupply investigatorId supply -> do
      let
        cost = supplyCost supply
        supplyMap =
          adjustMap
            (max 0 . subtract cost)
            investigatorId
            (supplyPoints metadata)
      pure
        . TheForgottenAge
        $ attrs
        `with` Metadata
          supplyMap
          (yithians metadata)
    PreScenarioSetup -> do
      pushAll $ map BecomeYithian $ toList $ yithians metadata
      pure c
    _ -> defaultCampaignRunner msg c
