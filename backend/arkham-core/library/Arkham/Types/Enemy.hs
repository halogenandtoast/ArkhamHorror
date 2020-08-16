{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , isEngaged
  , getEngagedInvestigators
  , getBearer
  , Enemy
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Cards.Acolyte
import Arkham.Types.Enemy.Cards.FleshEater
import Arkham.Types.Enemy.Cards.GhoulMinion
import Arkham.Types.Enemy.Cards.GhoulPriest
import Arkham.Types.Enemy.Cards.HuntingNightgaunt
import Arkham.Types.Enemy.Cards.IcyGhoul
import Arkham.Types.Enemy.Cards.RavenousGhoul
import Arkham.Types.Enemy.Cards.SilverTwilightAcolyte
import Arkham.Types.Enemy.Cards.SwarmOfRats
import Arkham.Types.Enemy.Cards.WizardOfTheOrder
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Prey
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy = fromJustNote "Unkown enemy" . flip HashMap.lookup allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = HashMap.fromList
  [ ("01102", SilverTwilightAcolyte' . silverTwilightAcolyte)
  , ("01116", GhoulPriest' . ghoulPriest)
  , ("01118", FleshEater' . fleshEater)
  , ("01119", IcyGhoul' . icyGhoul)
  , ("01159", SwarmOfRats' . swarmOfRats)
  , ("01160", GhoulMinion' . ghoulMinion)
  , ("01161", RavenousGhoul' . ravenousGhoul)
  , ("01169", Acolyte' . acolyte)
  , ("01170", WizardOfTheOrder' . wizardOfTheOrder)
  , ("01172", HuntingNightgaunt' . huntingNightgaunt)
  ]

isEngaged :: Enemy -> Bool
isEngaged = not . null . enemyEngagedInvestigators . enemyAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . enemyAttrs

getBearer :: Enemy -> Maybe InvestigatorId
getBearer enemy = case enemyPrey (enemyAttrs enemy) of
  Bearer iid -> Just (InvestigatorId $ unBearerId iid)
  _ -> Nothing

instance HasVictoryPoints Enemy where
  getVictoryPoints = enemyVictory . enemyAttrs

instance HasId LocationId () Enemy where
  getId _ = enemyLocation . enemyAttrs

instance HasCardCode Enemy where
  getCardCode = enemyCardCode . enemyAttrs

instance HasAbilities Enemy where
  getAbilities = enemyAbilities . enemyAttrs

instance HasTraits Enemy where
  getTraits = enemyTraits . enemyAttrs

instance HasKeywords Enemy where
  getKeywords = enemyKeywords . enemyAttrs

data Enemy
  = SilverTwilightAcolyte' SilverTwilightAcolyte
  | GhoulPriest' GhoulPriest
  | FleshEater' FleshEater
  | IcyGhoul' IcyGhoul
  | SwarmOfRats' SwarmOfRats
  | GhoulMinion' GhoulMinion
  | RavenousGhoul' RavenousGhoul
  | Acolyte' Acolyte
  | WizardOfTheOrder' WizardOfTheOrder
  | HuntingNightgaunt' HuntingNightgaunt
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

enemyAttrs :: Enemy -> Attrs
enemyAttrs = \case
  SilverTwilightAcolyte' attrs -> coerce attrs
  GhoulPriest' attrs -> coerce attrs
  FleshEater' attrs -> coerce attrs
  IcyGhoul' attrs -> coerce attrs
  SwarmOfRats' attrs -> coerce attrs
  GhoulMinion' attrs -> coerce attrs
  RavenousGhoul' attrs -> coerce attrs
  Acolyte' attrs -> coerce attrs
  WizardOfTheOrder' attrs -> coerce attrs
  HuntingNightgaunt' attrs -> coerce attrs

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg = \case
    SilverTwilightAcolyte' x -> SilverTwilightAcolyte' <$> runMessage msg x
    GhoulPriest' x -> GhoulPriest' <$> runMessage msg x
    FleshEater' x -> FleshEater' <$> runMessage msg x
    IcyGhoul' x -> IcyGhoul' <$> runMessage msg x
    SwarmOfRats' x -> SwarmOfRats' <$> runMessage msg x
    GhoulMinion' x -> GhoulMinion' <$> runMessage msg x
    RavenousGhoul' x -> RavenousGhoul' <$> runMessage msg x
    Acolyte' x -> Acolyte' <$> runMessage msg x
    WizardOfTheOrder' x -> WizardOfTheOrder' <$> runMessage msg x
    HuntingNightgaunt' x -> HuntingNightgaunt' <$> runMessage msg x
