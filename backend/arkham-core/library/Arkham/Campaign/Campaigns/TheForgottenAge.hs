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
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Token

newtype TheForgottenAge = TheForgottenAge CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theForgottenAge :: Difficulty -> TheForgottenAge
theForgottenAge difficulty = campaign
  TheForgottenAge
  (CampaignId "04")
  "The Forgotten Age"
  difficulty
  (chaosBagContents difficulty)

supplyPoints :: (Monad m, HasGame m) => m Int
supplyPoints = do
  n <- getPlayerCount
  pure $ case n of
    1 -> 10
    2 -> 7
    3 -> 5
    4 -> 4
    _ -> error "invalid player count"

supplyCost :: Supply -> Int
supplyCost = \case
  Provisions -> 1
  Medicine -> 2
  Rope -> 3
  Blanket -> 2
  Canteen -> 2
  Torches -> 3
  Compass -> 2
  Map -> 3
  Binoculars -> 2
  Chalk -> 2
  Pendant -> 1

supplyLabel :: Supply -> [Message] -> UI Message
supplyLabel s = case s of
  Provisions -> go
    "Provisions"
    "(1 supply point each): Food and water for one person. A must-have for any journey."
  Medicine -> go
    "Medicine"
    "(2 supply points each): To stave off disease, infection, or venom."
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
  where go label tooltip = TooltipLabel label (Tooltip tooltip)

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge attrs) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getInvestigatorIds
      let steps = [1 .. length investigatorIds]
      pushAll
        $ [story investigatorIds prologue]
        <> map (SetupStep CampaignTarget) steps
        <> [NextCampaignStep Nothing]
      pure c
    SetupStep CampaignTarget n -> do
      investigatorIds <- getInvestigatorIds
      totalSupplyPoints <- supplyPoints
      let
        investigatorId =
          fromJustNote "invalid setup step" (investigatorIds !!? (n - 1))
      investigatorSupplies <- field InvestigatorSupplies investigatorId
      let
        remaining = totalSupplyPoints
          - getSum (foldMap (Sum . supplyCost) investigatorSupplies)

      when (remaining > 0) $ do
        let
          availableSupply s =
            s `notElem` investigatorSupplies || s `elem` [Provisions, Medicine]
          affordableSupplies = filter ((<= remaining) . supplyCost) allSupplies
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
                [PickSupply investigatorId s, SetupStep CampaignTarget n]
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
                 [ targetLabel
                     iid
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
                 [ targetLabel
                     iid
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
                 : [ targetLabel
                       poisoned
                       [ RemoveCampaignCardFromDeck poisoned "04102"
                       , UseSupply doctor Medicine
                       ]
                   | (poisoned, doctor) <- zip withPoisoned withMedicine
                   ]
               ]
             else []
           )
        <> [CampaignStep (Just (InterludeStepPart 1 mkey 2))]
      pure c
    CampaignStep (Just (InterludeStepPart 1 _ 2)) -> do
      let
        withPoisoned =
          flip mapMaybe (mapToList $ campaignDecks attrs)
            $ \(iid, Deck cards) ->
                if any (`cardMatch` CardWithTitle "Poisoned") cards
                  then Just iid
                  else Nothing
      pushAll
        $ (if notNull withPoisoned
            then
              story withPoisoned thePoisonSpreads
                : [ SufferTrauma iid 1 0 | iid <- withPoisoned ]
            else []
          )
        <> [NextCampaignStep Nothing]
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
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push (CampaignStep step)
      pure
        . TheForgottenAge
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    HandleTargetChoice _ CampaignSource (InvestigatorTarget iid) -> do
      pure
        . TheForgottenAge
        $ attrs
        & (modifiersL %~ insertWith
            (<>)
            iid
            [toModifier CampaignSource $ StartingResources (-3)]
          )
    EndOfScenario _ -> do
      pure . TheForgottenAge $ attrs & modifiersL .~ mempty
    _ -> TheForgottenAge <$> runMessage msg attrs
