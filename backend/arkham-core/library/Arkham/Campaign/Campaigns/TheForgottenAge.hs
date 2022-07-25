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
import Arkham.Message

newtype TheForgottenAge = TheForgottenAge CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theForgottenAge :: Difficulty -> TheForgottenAge
theForgottenAge difficulty = TheForgottenAge $ baseAttrs
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

data Supply = Supply Text Int Text

supplies :: [Supply]
supplies =
  [ Supply
    "Provisions"
    1
    "Food and water for one person. A must-have for any journey."
  , Supply "Medicine" 2 "To stave off disease, infection, or venom."
  , Supply
    "Rope"
    3
    "Several long coils of strong rope.  Vital for climbing and spelunking."
  , Supply "Blanket" 2 "For warmth at night."
  , Supply "Canteen" 2 "Can be refilled at streams and rivers."
  , Supply "Torches" 3 "Can light up dark areas, or set sconces alight."
  , Supply "Compass" 2 "Can guide you when you are hopelessly lost."
  , Supply
    "Map"
    3
    "Unmarked for now, but with time, you may be able to map out your surroundings."
  , Supply "Binoculars" 2 "To help you see faraway places."
  , Supply "Chalk" 2 "For writing on rough stone surfaces."
  , Supply
    "Pendant"
    1
    "Useless, but fond memories bring comfort to travelers far from home."
  ]

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge attrs) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getInvestigatorIds
      pushAll $ [story investigatorIds prologue] <> [NextCampaignStep Nothing]
      pure c
    _ -> TheForgottenAge <$> runMessage msg attrs
