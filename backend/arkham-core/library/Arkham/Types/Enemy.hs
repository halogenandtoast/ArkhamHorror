{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , baseEnemy
  , isEngaged
  , getEngagedInvestigators
  , getBearer
  , Enemy
  )
where

import Arkham.Import

import Arkham.Types.Helpers
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Cards
import Arkham.Types.Enemy.Runner
import Arkham.Types.Prey
import Data.Coerce

data Enemy
  = MobEnforcer' MobEnforcer
  | SilverTwilightAcolyte' SilverTwilightAcolyte
  | StubbornDetective' StubbornDetective
  | GhoulPriest' GhoulPriest
  | FleshEater' FleshEater
  | IcyGhoul' IcyGhoul
  | TheMaskedHunter' TheMaskedHunter
  | WolfManDrew' WolfManDrew
  | HermanCollins' HermanCollins
  | PeterWarren' PeterWarren
  | VictoriaDevereux' VictoriaDevereux
  | RuthTurner' RuthTurner
  | Umordhoth' Umordhoth
  | SwarmOfRats' SwarmOfRats
  | GhoulMinion' GhoulMinion
  | RavenousGhoul' RavenousGhoul
  | Acolyte' Acolyte
  | WizardOfTheOrder' WizardOfTheOrder
  | HuntingNightgaunt' HuntingNightgaunt
  | ScreechingByakhee' ScreechingByakhee
  | YithianObserver' YithianObserver
  | RelentlessDarkYoung' RelentlessDarkYoung
  | GoatSpawn' GoatSpawn
  | YoungDeepOne' YoungDeepOne
  | BogGator' BogGator
  | SwampLeech' SwampLeech
  | TheRougarou' TheRougarou
  | BaseEnemy' BaseEnemy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BaseEnemy = BaseEnemy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseEnemy :: EnemyId -> CardCode -> (Attrs -> Attrs) -> Enemy
baseEnemy a b f = BaseEnemy' . BaseEnemy . f $ baseAttrs a b

instance ActionRunner env => HasActions env BaseEnemy where
  getActions investigator window (BaseEnemy attrs) =
    getActions investigator window attrs

instance HasModifiersFor env BaseEnemy where
  getModifiersFor _ _ _ = pure []

instance HasModifiers env BaseEnemy where
  getModifiers _ (BaseEnemy Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance (EnemyRunner env) => RunMessage env BaseEnemy where
  runMessage msg (BaseEnemy attrs) = BaseEnemy <$> runMessage msg attrs

deriving anyclass instance ActionRunner env => HasActions env Enemy
deriving anyclass instance EnemyRunner env => HasModifiersFor env Enemy

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg e = do
    modifiers' <- getModifiers (EnemySource (getId () e)) e
    if any isBlank modifiers' && not (isBlanked msg)
      then runMessage (Blanked msg) e
      else defaultRunMessage msg e

instance HasVictoryPoints Enemy where
  getVictoryPoints = enemyVictory . enemyAttrs

instance HasCount DoomCount () Enemy where
  getCount _ = DoomCount . enemyDoom . enemyAttrs

instance HasCount HealthDamageCount () Enemy where
  getCount _ = HealthDamageCount . enemyHealthDamage . enemyAttrs

instance HasCount SanityDamageCount () Enemy where
  getCount _ = SanityDamageCount . enemySanityDamage . enemyAttrs

instance HasId LocationId () Enemy where
  getId _ = enemyLocation . enemyAttrs

instance HasSet TreacheryId () Enemy where
  getSet _ = enemyTreacheries . enemyAttrs

instance HasSet AssetId () Enemy where
  getSet _ = enemyAssets . enemyAttrs

instance HasCardCode Enemy where
  getCardCode = enemyCardCode . enemyAttrs

instance HasTraits Enemy where
  getTraits = enemyTraits . enemyAttrs

instance HasKeywords Enemy where
  getKeywords = enemyKeywords . enemyAttrs

deriving anyclass instance HasCount RemainingSanity InvestigatorId env => HasModifiers env Enemy

instance HasId EnemyId () Enemy where
  getId _ = enemyId . enemyAttrs

instance IsEnemy Enemy where
  isAloof = isAloof . enemyAttrs

instance HasDamage Enemy where
  getDamage = (, 0) . enemyDamage . enemyAttrs

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy = fromJustNote "Unkown enemy" . flip lookup allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = mapFromList
  [ ("01101", MobEnforcer' . mobEnforcer)
  , ("01102", SilverTwilightAcolyte' . silverTwilightAcolyte)
  , ("01103", StubbornDetective' . stubbornDetective)
  , ("01116", GhoulPriest' . ghoulPriest)
  , ("01118", FleshEater' . fleshEater)
  , ("01119", IcyGhoul' . icyGhoul)
  , ("01121b", TheMaskedHunter' . theMaskedHunter)
  , ("01137", WolfManDrew' . wolfManDrew)
  , ("01138", HermanCollins' . hermanCollins)
  , ("01139", PeterWarren' . peterWarren)
  , ("01140", VictoriaDevereux' . victoriaDevereux)
  , ("01141", RuthTurner' . ruthTurner)
  , ("01157", Umordhoth' . umordhoth)
  , ("01159", SwarmOfRats' . swarmOfRats)
  , ("01160", GhoulMinion' . ghoulMinion)
  , ("01161", RavenousGhoul' . ravenousGhoul)
  , ("01169", Acolyte' . acolyte)
  , ("01170", WizardOfTheOrder' . wizardOfTheOrder)
  , ("01172", HuntingNightgaunt' . huntingNightgaunt)
  , ("01175", ScreechingByakhee' . screechingByakhee)
  , ("01177", YithianObserver' . yithianObserver)
  , ("01179", RelentlessDarkYoung' . relentlessDarkYoung)
  , ("01180", GoatSpawn' . goatSpawn)
  , ("01181", YoungDeepOne' . youngDeepOne)
  , ("81022", BogGator' . bogGator)
  , ("81023", SwampLeech' . swampLeech)
  , ("81028", TheRougarou' . theRougarou)
  , ("enemy", \eid -> baseEnemy eid "enemy" id)
  ]

isEngaged :: Enemy -> Bool
isEngaged = not . null . enemyEngagedInvestigators . enemyAttrs

instance Exhaustable Enemy where
  isExhausted = enemyExhausted . enemyAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . enemyAttrs

getBearer :: Enemy -> Maybe InvestigatorId
getBearer enemy = case enemyPrey (enemyAttrs enemy) of
  Bearer iid -> Just (InvestigatorId $ unBearerId iid)
  _ -> Nothing

isBlanked :: Message -> Bool
isBlanked Blanked{} = True
isBlanked _ = False

enemyAttrs :: Enemy -> Attrs
enemyAttrs = \case
  MobEnforcer' attrs -> coerce attrs
  SilverTwilightAcolyte' attrs -> coerce attrs
  StubbornDetective' attrs -> coerce attrs
  GhoulPriest' attrs -> coerce attrs
  FleshEater' attrs -> coerce attrs
  IcyGhoul' attrs -> coerce attrs
  TheMaskedHunter' attrs -> coerce attrs
  WolfManDrew' attrs -> coerce attrs
  HermanCollins' attrs -> coerce attrs
  PeterWarren' attrs -> coerce attrs
  VictoriaDevereux' attrs -> coerce attrs
  RuthTurner' attrs -> coerce attrs
  Umordhoth' attrs -> coerce attrs
  SwarmOfRats' attrs -> coerce attrs
  GhoulMinion' attrs -> coerce attrs
  RavenousGhoul' attrs -> coerce attrs
  Acolyte' attrs -> coerce attrs
  WizardOfTheOrder' attrs -> coerce attrs
  HuntingNightgaunt' attrs -> coerce attrs
  ScreechingByakhee' attrs -> coerce attrs
  YithianObserver' attrs -> coerce attrs
  RelentlessDarkYoung' attrs -> coerce attrs
  GoatSpawn' attrs -> coerce attrs
  YoungDeepOne' attrs -> coerce attrs
  BogGator' attrs -> coerce attrs
  SwampLeech' attrs -> coerce attrs
  TheRougarou' (TheRougarou (attrs `With` _)) -> attrs
  BaseEnemy' attrs -> coerce attrs
