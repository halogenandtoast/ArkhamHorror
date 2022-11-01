module Arkham.Campaign.Campaigns.TheForgottenAge
  ( TheForgottenAge(..)
  , theForgottenAge
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Control.Newtype ( ala )
import Data.Monoid ( Endo (..) )

data Metadata = Metadata
  { supplyPoints :: HashMap InvestigatorId Int
  , yithians :: HashSet InvestigatorId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Metadata where
  Metadata a1 b1 <> Metadata a2 b2 = Metadata (a1 <> a2) (b1 <> b2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty

newtype TheForgottenAge = TheForgottenAge (CampaignAttrs `With` Metadata)
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theForgottenAge :: Difficulty -> TheForgottenAge
theForgottenAge difficulty = campaign
  (TheForgottenAge . (`with` mempty))
  (CampaignId "04")
  "The Forgotten Age"
  difficulty
  (chaosBagContents difficulty)

initialSupplyPoints :: (Monad m, HasGame m) => m Int
initialSupplyPoints = getPlayerCountValue (ByPlayerCount 10 7 5 4)

initialResupplyPoints :: (Monad m, HasGame m) => m Int
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
  Provisions -> go
    "Provisions"
    "(1 supply point each): Food and water for one person. A must-have for any journey."
  Medicine -> go
    "Medicine"
    "(2 supply points each): To stave off disease, infection, or venom."
  Gasoline ->
    go "Gasoline" "(2 supply points each): Enough for a long journey by car."
  Rope -> go
    "Rope"
    "(3 supply points): Several long coils of strong rope.  Vital for climbing and spelunking."
  Blanket -> go "Blanket" "(2 supply points): For warmth at night."
  Canteen ->
    go "Canteen" "(2 supply points): Can be refilled at streams and rivers."
  Torches -> go
    "Torches"
    "(3 supply points): Can light up dark areas, or set sconces alight."
  Compass -> go
    "Compass"
    "(2 supply points): Can guide you when you are hopelessly lost."
  Map -> go
    "Map"
    "(3 supply points): Unmarked for now, but with time, you may be able to map out your surroundings."
  Binoculars ->
    go "Binoculars" "(2 supply points): To help you see faraway places."
  Chalk -> go "Chalk" "(2 supply points): For writing on rough stone surfaces."
  Pendant -> go
    "Pendant"
    "(1 supply point): Useless, but fond memories bring comfort to travelers far from home."
  Pocketknife -> go
    "Pocketknife"
    "(2 supply point): Too small to be used as a reliable weapon, but easily concealed."
  Pickaxe ->
    go "Pickaxe" "(2 supply point): For breaking apart rocky surfaces."
  where go label tooltip = TooltipLabel label (Tooltip tooltip)

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge (attrs `With` metadata)) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getInvestigatorIds
      totalSupplyPoints <- initialSupplyPoints
      let supplyMap = mapFromList $ map (, totalSupplyPoints) investigatorIds
      pushAll
        $ [story investigatorIds prologue]
        <> [ CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep))
           | iid <- investigatorIds
           ]
        <> [NextCampaignStep Nothing]
      pure . TheForgottenAge $ attrs `with` Metadata
        supplyMap
        (yithians metadata)
    CampaignStep (Just (InvestigatorCampaignStep investigatorId PrologueStep))
      -> do
        let
          remaining = findWithDefault 0 investigatorId (supplyPoints metadata)
        investigatorSupplies <- field InvestigatorSupplies investigatorId

        when (remaining > 0) $ do
          let
            availableSupply s =
              s
                `notElem` investigatorSupplies
                || s
                `elem` [Provisions, Medicine]
            affordableSupplies =
              filter ((<= remaining) . supplyCost) prologueSupplies
            availableSupplies = filter availableSupply affordableSupplies
          push
            $ Ask investigatorId
            $ QuestionLabel
                ("Available Supplies ("
                <> tshow remaining
                <> " supply points remaining)"
                )
            $ ChooseOne
            $ Label "Done" []
            : map
                (\s -> supplyLabel
                  s
                  [ PickSupply investigatorId s
                  , CampaignStep
                    (Just $ InvestigatorCampaignStep investigatorId PrologueStep
                    )
                  ]
                )
                availableSupplies

        pure c
    CampaignStep (Just (InterludeStep 1 mkey)) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      withBlanket <- getInvestigatorsWithSupply Blanket
      withoutBlanket <- getInvestigatorsWithoutSupply Blanket
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
      provisions <- concat <$> for
        investigatorIds
        \iid -> do
          provisions <- fieldMap
            InvestigatorSupplies
            (filter (== Provisions))
            iid
          pure $ map (iid, ) provisions
      investigatorsWithBinocularsPairs <- for investigatorIds $ \iid -> do
        binoculars <- fieldMap InvestigatorSupplies (any (== Provisions)) iid
        pure (iid, binoculars)
      let
        lowOnRationsCount = length investigatorIds - length provisions
        useProvisions = take (length investigatorIds) provisions
      pushAll
        $ [ story withBlanket restfulSleep | notNull withBlanket ]
        <> concatMap
             (\iid ->
               [ story [iid] tossingAndTurning
               , chooseOne
                 iid
                 [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                 , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                 ]
               ]
             )
             withoutBlanket
        <> map (uncurry UseSupply) useProvisions
        <> [ Ask leadInvestigatorId
             $ QuestionLabel
                 "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
             $ ChooseN
                 lowOnRationsCount
                 [ CardLabel
                     (unInvestigatorId iid)
                     [ story [iid] lowOnRations
                     , HandleTargetChoice
                       iid
                       CampaignSource
                       (InvestigatorTarget iid)
                     ]
                 | iid <- investigatorIds
                 ]
           | lowOnRationsCount > 0
           ]
        <> [ Ask leadInvestigatorId
             $ QuestionLabel
                 "The lead investigator must choose one investigator to be the group’s lookout. Then, that investigator checks his or her supplies. If he or she has binoculars, he or she reads Shapes in the Trees. Otherwise, he or she reads Eyes in the Dark."
             $ ChooseOne
                 [CardLabel
                     (unInvestigatorId iid)
                     (if hasBinoculars
                       then [story [iid] shapesInTheTrees, GainXP iid 2]
                       else [story [iid] eyesInTheDark, SufferTrauma iid 0 1]
                     )
                 | (iid, hasBinoculars) <- investigatorsWithBinocularsPairs
                 ]
           ]
        <> (if notNull withMedicine && notNull withPoisoned
             then
               [ Ask leadInvestigatorId
                 $ QuestionLabel
                     "Choose an investigator to removed poisoned by using a medicine"
                 $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
                 $ Done "Do not use medicine"
                 : [ CardLabel
                       (unInvestigatorId poisoned)
                       [ RemoveCampaignCardFromDeck
                         poisoned
                         Treacheries.poisoned
                       , UseSupply doctor Medicine
                       ]
                   | (poisoned, doctor) <- zip withPoisoned withMedicine
                   ]
               ]
             else []
           )
        <> [CampaignStep (Just (InterludeStepPart 1 mkey 2))]
        <> [NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InterludeStepPart 1 _ 2)) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
                if any (`cardMatch` CardWithTitle "Poisoned") cards
                  then Just iid
                  else Nothing
      pushAll $ if notNull withPoisoned
        then
          story withPoisoned thePoisonSpreads
            : [ SufferTrauma iid 1 0 | iid <- withPoisoned ]
        else []
      pure c
    CampaignStep (Just (InterludeStep 2 mkey)) -> do
      recoveredTheRelicOfAges <- getHasRecord
        TheInvestigatorsRecoveredTheRelicOfAges
      let expeditionsEndStep = if recoveredTheRelicOfAges then 1 else 5
      push $ CampaignStep (Just (InterludeStepPart 2 mkey expeditionsEndStep))
      pure c
    CampaignStep (Just (InterludeStepPart 2 mkey 1)) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      pushAll
        [ story investigatorIds expeditionsEnd1
        , chooseOne
          leadInvestigatorId
          [ Label
            "It belongs in a museum. Alejandro and the museum staff will be able to study it and learn more about its purpose. - Proceed to Expedition’s End 2."
            [CampaignStep (Just (InterludeStepPart 2 mkey 2))]
          , Label
            "It is too dangerous to be on display. We should keep it hidden and safe until we know more about it. - Skip to Expedition's End 3."
            [CampaignStep (Just (InterludeStepPart 2 mkey 3))]
          ]
        ]
      pure c
    CampaignStep (Just (InterludeStepPart 2 mkey 2)) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      let
        inADeckAlready =
          any ((== Assets.alejandroVela) . toCardDef)
            . concat
            . toList
            $ campaignStoryCards attrs
      pushAll
        $ [ story investigatorIds expeditionsEnd2
          , Record TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
          , Record TheInvestigatorsHaveEarnedAlejandrosTrust
          ]
        <> [ addCampaignCardToDeckChoice
               leadInvestigatorId
               investigatorIds
               Assets.alejandroVela
           | not inADeckAlready
           ]
        <> [AddToken Tablet, CampaignStep (Just (InterludeStepPart 2 mkey 4))]
      pure c
    CampaignStep (Just (InterludeStepPart 2 mkey 3)) -> do
      investigatorIds <- getInvestigatorIds
      pushAll
        [ story investigatorIds expeditionsEnd3
        , Record TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
        , Record AlejandroIsContinuingHisResearchOnHisOwn
        , CampaignStep (Just (InterludeStepPart 2 mkey 4))
        ]
      pure c
    CampaignStep (Just (InterludeStepPart 2 _ 4)) -> do
      investigatorIds <- getInvestigatorIds
      pushAll [story investigatorIds expeditionsEnd4, NextCampaignStep Nothing]
      pure c
    CampaignStep (Just (InterludeStepPart 2 _ 5)) -> do
      investigatorIds <- getInvestigatorIds
      pushAll [story investigatorIds expeditionsEnd5, NextCampaignStep Nothing]
      pure c
    CampaignStep (Just ResupplyPoint) -> do
      investigatorIds <- getInvestigatorIds
      totalResupplyPoints <- initialResupplyPoints
      poisonedInvestigators <- filterM getIsPoisoned investigatorIds
      poisonedInvestigatorsWith3Xp <- filterM
        (fieldP InvestigatorXp (>= 3))
        poisonedInvestigators

      investigatorsWhoCanHealTrauma <- catMaybes <$> for
        investigatorIds
        \iid -> do
          hasPhysicalTrauma <- fieldP InvestigatorPhysicalTrauma (> 0) iid
          hasMentalTrauma <- fieldP InvestigatorMentalTrauma (> 0) iid
          hasXp <- fieldP InvestigatorXp (>= 5) iid
          if (hasPhysicalTrauma || hasMentalTrauma) && hasXp
            then pure $ Just (iid, hasPhysicalTrauma, hasMentalTrauma)
            else pure Nothing

      let
        resupplyMap = mapFromList $ map (, totalResupplyPoints) investigatorIds

      pushAll
        $ [ chooseOne
              iid
              [ Label
                "Spend 3 xp to visit St. Mary's Hospital and remove a poisoned weakness"
                [ SpendXP iid 3
                , RemoveCampaignCardFromDeck iid Treacheries.poisoned
                ]
              , Label "Do not remove poisoned weakness" []
              ]
          | iid <- poisonedInvestigatorsWith3Xp
          ]
        <> [ chooseOne iid
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
           | (iid, hasPhysical, hasMental) <- investigatorsWhoCanHealTrauma
           ]
        <> [ CampaignStep (Just (InvestigatorCampaignStep iid ResupplyPoint))
           | iid <- investigatorIds
           ]
        <> [NextCampaignStep Nothing]
      pure . TheForgottenAge $ attrs `with` Metadata
        resupplyMap
        (yithians metadata)
    CampaignStep (Just (InvestigatorCampaignStep investigatorId ResupplyPoint))
      -> do
        let
          remaining = findWithDefault 0 investigatorId (supplyPoints metadata)
        investigatorSupplies <- field InvestigatorSupplies investigatorId
        when (remaining > 0) $ do
          let
            availableSupply s =
              s
                `notElem` investigatorSupplies
                || s
                `elem` [Provisions, Medicine, Gasoline]
            affordableSupplies =
              filter ((<= remaining) . supplyCost) resupplyPointSupplies
            availableSupplies = filter availableSupply affordableSupplies
          push
            $ Ask investigatorId
            $ QuestionLabel
                ("Available Supplies ("
                <> tshow remaining
                <> " supply points remaining)"
                )
            $ ChooseOne
            $ Label "Done" []
            : map
                (\s -> supplyLabel
                  s
                  [ PickSupply investigatorId s
                  , CampaignStep
                    (Just
                    $ InvestigatorCampaignStep investigatorId ResupplyPoint
                    )
                  ]
                )
                availableSupplies

        pure c
    CampaignStep (Just (InterludeStep 3 mkey)) -> do
      iids <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      withGasoline <- getInvestigatorsWithSupply Gasoline
      withCanteen <- getInvestigatorsWithSupply Canteen
      withMap <- getInvestigatorsWithSupply Map
      isFaithRestored <- andM
        [ getHasRecord TheInvestigatorsForgedABondWithIchtaca
        , getHasRecord IchtacaHasConfidenceInYou
        , pure $ count (== Cultist) (campaignChaosBag attrs) >= 2
        ]
      provisions <- concat <$> for
        iids
        \iid -> do
          provisions <- fieldMap
            InvestigatorSupplies
            (filter (== Provisions))
            iid
          pure $ map (iid, ) provisions
      withMedicine <- flip concatMapM iids $ \iid -> do
        n <- getSupplyCount iid Medicine
        pure $ replicate n iid
      let
        (gasMessages, gasUpdate) = case withGasoline of
          [] ->
            ( [story iids outOfGas]
            , ala Endo foldMap
              $ [ modifiersL %~ insertWith
                    (<>)
                    iid
                    [toModifier CampaignSource CannotMulligan]
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
          [] -> ([story iids secretsInTheStone], id)
          xs ->
            ( [story iids patternsInTheStone]
            , ala
              Endo
              foldMap
              [ modifiersL %~ insertWith
                  (<>)
                  iid
                  [toModifier CampaignSource $ StartingClues 1]
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
        $ story iids theJungleBeckons
        : gasMessages
        <> mapMessages
        <> map (uncurry UseSupply) useProvisions
        <> [ Ask leadInvestigatorId
             $ QuestionLabel
                 "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
             $ ChooseN
                 lowOnRationsCount
                 [ CardLabel
                     (unInvestigatorId iid)
                     [ story [iid] lowOnRationsInterlude3
                     , HandleTargetChoice
                       iid
                       CampaignSource
                       (InvestigatorTarget iid)
                     ]
                 | iid <- iids
                 ]
           | lowOnRationsCount > 0
           ]
        <> (if notNull withMedicine && notNull withPoisoned
             then
               [ Ask leadInvestigatorId
                 $ QuestionLabel
                     "Choose an investigator to removed poisoned by using a medicine"
                 $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
                 $ Done "Do not use medicine"
                 : [ CardLabel
                       (unInvestigatorId poisoned)
                       [ RemoveCampaignCardFromDeck
                         poisoned
                         Treacheries.poisoned
                       , UseSupply doctor Medicine
                       ]
                   | (poisoned, doctor) <- zip withPoisoned withMedicine
                   ]
               ]
             else []
           )
        <> [CampaignStep (Just (InterludeStepPart 3 mkey 2))]
        <> canteenMessages
        <> (if isFaithRestored
             then [Record IchtacasFaithIsRestored, AddToken Cultist]
             else []
           )
        <> [NextCampaignStep Nothing]
      pure
        . TheForgottenAge
        . (`with` metadata)
        $ attrs
        & gasUpdate
        & canteenUpdate
    CampaignStep (Just (InterludeStepPart 3 _ 2)) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
                if any (`cardMatch` CardWithTitle "Poisoned") cards
                  then Just iid
                  else Nothing
      pushAll $ if notNull withPoisoned
        then
          story withPoisoned thePoisonSpreadsInterlude3
            : [ SufferTrauma iid 1 0 | iid <- withPoisoned ]
        else []
      pure c
    CampaignStep (Just (InterludeStep 4 mkey)) -> do
      pushAll
        [ CampaignStep (Just (InterludeStepPart 4 mkey 1))
        , CampaignStep (Just (InterludeStepPart 4 mkey 2))
        , CampaignStep (Just (InterludeStepPart 4 mkey 3))
        , CampaignStep (Just (InterludeStepPart 4 mkey 4))
        , CampaignStep (Just (InterludeStepPart 4 mkey 5))
        , CampaignStep (Just (InterludeStepPart 4 mkey 6))
        , NextCampaignStep Nothing
        ]
      pure c
    CampaignStep (Just (InterludeStepPart 4 _ 1)) -> do
      backfired <- getHasRecord TheProcessBackfired
      backfiredSpectacularly <- getHasRecord TheProcessBackfiredSpectacularly
      iids <- getInvestigatorIds
      -- no chaos bag technically so we sample from campaign
      let
        chaosBag =
          fromJustNote "missing tokens" $ nonEmpty $ campaignChaosBag attrs
      if backfired || backfiredSpectacularly
        then do
          results <- for iids $ \iid -> do
            tokens <- sampleN (if backfired then 1 else 2) chaosBag
            let
              outOfBody = any
                (\t ->
                  t
                    `elem` [Cultist, Tablet, ElderThing, AutoFail, Skull]
                    || (t /= PlusOne && isNumberToken t)
                )
                tokens
              stuckAsYithian =
                any (`elem` [Cultist, Tablet, ElderThing, AutoFail]) tokens
            pure (iid, outOfBody, stuckAsYithian)

          let
            yithians = setFromList $ mapMaybe
              (\(iid, _, stuckAsYithian) ->
                if stuckAsYithian then Just iid else Nothing
              )
              results

          pushAll
            [ AddCampaignCardToDeck iid Treacheries.outOfBodyExperience
            | (iid, outOfBody, _) <- results
            , outOfBody
            ]
          pure
            . TheForgottenAge
            $ attrs
            `With` Metadata (supplyPoints metadata) yithians
        else pure c
    CampaignStep (Just (InterludeStepPart 4 mkey 2)) -> do
      iids <- getInvestigatorIds
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      let
        allMet = and
          [ count (== Tablet) (campaignChaosBag attrs) == 2
          , rescuedAlejandro
          , mkey == Just TheCustodianWasUnderControl
          ]
      pushAll $ if allMet
        then
          [ story iids aMindRecovered
          , Record AlejandroRemembersEverything
          , AddToken Tablet
          ]
        else
          [ story iids foreverLost
          , Record AlejandroIsSetAgainstYou
          , RemoveCampaignCard Assets.alejandroVela
          ]
      pure c
    CampaignStep (Just (InterludeStepPart 4 _ 3)) -> do
      hasChalk <- getAnyHasSupply Chalk
      iids <- getInvestigatorIds
      let storyEntry = if hasChalk then theWayIsOpen else theWayIsShut
      push $ story iids storyEntry

      let
        update = if hasChalk
          then id
          else
            ala Endo foldMap
              $ [ modifiersL %~ insertWith
                    (<>)
                    iid
                    [toModifier CampaignSource CannotMulligan]
                | iid <- iids
                ]
      pure . TheForgottenAge . (`with` metadata) $ attrs & update
    CampaignStep (Just (InterludeStepPart 4 _ 4)) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
      provisions <- concat <$> for
        investigatorIds
        \iid -> do
          provisions <- fieldMap
            InvestigatorSupplies
            (filter (== Provisions))
            iid
          pure $ map (iid, ) provisions
      let
        lowOnRationsCount = length investigatorIds - length provisions
        useProvisions = take (length investigatorIds) provisions
      pushAll
        $ map (uncurry UseSupply) useProvisions
        <> [ Ask leadInvestigatorId
             $ QuestionLabel
                 "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
             $ ChooseN
                 lowOnRationsCount
                 [ CardLabel
                     (unInvestigatorId iid)
                     [ story [iid] lowOnRationsInterlude4
                     , HandleTargetChoice
                       iid
                       CampaignSource
                       (InvestigatorTarget iid)
                     ]
                 | iid <- investigatorIds
                 ]
           | lowOnRationsCount > 0
           ]
      pure c
    CampaignStep (Just (InterludeStepPart 4 mkey 5)) -> do
      investigatorIds <- getInvestigatorIds
      leadInvestigatorId <- getLeadInvestigatorId
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
        $ (if notNull withMedicine && notNull withPoisoned
            then
              [ Ask leadInvestigatorId
                $ QuestionLabel
                    "Choose an investigator to removed poisoned by using a medicine"
                $ ChooseUpToN (min (length withMedicine) (length withPoisoned))
                $ Done "Do not use medicine"
                : [ CardLabel
                      (unInvestigatorId poisoned)
                      [ RemoveCampaignCardFromDeck poisoned Treacheries.poisoned
                      , UseSupply doctor Medicine
                      ]
                  | (poisoned, doctor) <- zip withPoisoned withMedicine
                  ]
              ]
            else []
          )
        <> [CampaignStep (Just (InterludeStepPart 4 mkey 51))]
      pure c
    CampaignStep (Just (InterludeStepPart 4 _ 51)) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
                if any (`cardMatch` CardWithTitle "Poisoned") cards
                  then Just iid
                  else Nothing
      pushAll $ if notNull withPoisoned
        then
          story withPoisoned thePoisonSpreadsInterlude4
            : [ SufferTrauma iid 1 0 | iid <- withPoisoned ]
        else []
      pure c
    CampaignStep (Just (InterludeStepPart 4 _ 6)) -> do
      withBlanket <- getInvestigatorsWithSupply Blanket
      withoutBlanket <- getInvestigatorsWithoutSupply Blanket
      pushAll
        $ [ story withBlanket restfulSleepInterlude4 | notNull withBlanket ]
        <> concatMap
             (\iid ->
               [ story [iid] tossingAndTurningInterlude4
               , chooseOne
                 iid
                 [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                 , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                 ]
               ]
             )
             withoutBlanket
      pure c
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push (CampaignStep step)
      pure
        . TheForgottenAge
        . (`with` metadata)
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    HandleTargetChoice _ CampaignSource (InvestigatorTarget iid) -> do
      pure
        . TheForgottenAge
        . (`with` metadata)
        $ attrs
        & (modifiersL %~ insertWith
            (<>)
            iid
            [toModifier CampaignSource $ StartingResources (-3)]
          )
    EndOfScenario _ -> do
      pure . TheForgottenAge . (`with` metadata) $ attrs & modifiersL .~ mempty
    PickSupply investigatorId supply -> do
      let
        cost = supplyCost supply
        supplyMap = adjustMap
          (max 0 . subtract cost)
          investigatorId
          (supplyPoints metadata)
      pure . TheForgottenAge $ attrs `with` Metadata
        supplyMap
        (yithians metadata)
    _ -> TheForgottenAge . (`with` metadata) <$> runMessage msg attrs
