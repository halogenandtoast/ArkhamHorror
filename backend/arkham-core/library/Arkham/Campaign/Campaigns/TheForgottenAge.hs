module Arkham.Campaign.Campaigns.TheForgottenAge
  ( TheForgottenAge(..)
  , theForgottenAge
  ) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype TheForgottenAge = TheForgottenAge CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

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

supplyLabel :: Supply -> [Message] -> Message
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
    CampaignStep (Just (InterludeStep 1 _)) -> do
      withBlanket <- getInvestigatorsWithSupply Blanket
      withoutBlanket <- getInvestigatorsWithoutSupply Blanket
      provisions <- do
        iids <- selectList Anyone
        concat <$> for
          iids
          \iid -> do
            provisions <- fieldMap
              InvestigatorSupplies
              (filter (== Provisions))
              iid
            pure $ map (iid, ) provisions
      investigatorsWithBinocularsPairs <- do
        iids <- selectList Anyone
        for iids $ \iid -> do
          binoculars <- fieldMap InvestigatorSupplies (any (== Provisions)) iid
          pure (iid, binoculars)
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      let
        lowOnRationsCount = length investigatorIds - length provisions
        useProvisions = take (length investigatorIds) provisions
      pushAll
        $ [ story withBlanket restfulSleep | notNull withBlanket ]
        <> [ story withoutBlanket tossingAndTurning | notNull withoutBlanket ]
        <> map (uncurry UseSupply) useProvisions
        <> [ Ask leadInvestigatorId
             $ QuestionLabel
                 "Check your supplies. The investigators, as a group, must cross off one provisions per investigator from their supplies. For each provisions they cannot cross off, choose an investigator to read Low on Rations"
             $ ChooseN
                 lowOnRationsCount
                 [ targetLabel iid [story [iid] lowOnRations]
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
        <> [NextCampaignStep Nothing]
      pure c
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push (CampaignStep step)
      pure
        . TheForgottenAge
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    _ -> TheForgottenAge <$> runMessage msg attrs
