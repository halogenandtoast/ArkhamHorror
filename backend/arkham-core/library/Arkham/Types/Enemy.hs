{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , isEngaged
  , isExhausted
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
import Arkham.Types.Enemy.Cards.GoatSpawn
import Arkham.Types.Enemy.Cards.HermanCollins
import Arkham.Types.Enemy.Cards.HuntingNightgaunt
import Arkham.Types.Enemy.Cards.IcyGhoul
import Arkham.Types.Enemy.Cards.MobEnforcer
import Arkham.Types.Enemy.Cards.PeterWarren
import Arkham.Types.Enemy.Cards.RavenousGhoul
import Arkham.Types.Enemy.Cards.RelentlessDarkYoung
import Arkham.Types.Enemy.Cards.RuthTurner
import Arkham.Types.Enemy.Cards.ScreechingByakhee
import Arkham.Types.Enemy.Cards.SilverTwilightAcolyte
import Arkham.Types.Enemy.Cards.StubbornDetective
import Arkham.Types.Enemy.Cards.SwarmOfRats
import Arkham.Types.Enemy.Cards.TheMaskedHunter
import Arkham.Types.Enemy.Cards.Umordhoth
import Arkham.Types.Enemy.Cards.VictoriaDevereux
import Arkham.Types.Enemy.Cards.WizardOfTheOrder
import Arkham.Types.Enemy.Cards.WolfManDrew
import Arkham.Types.Enemy.Cards.YithianObserver
import Arkham.Types.Enemy.Cards.YoungDeepOne
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import ClassyPrelude
import Data.Coerce
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
import Safe (fromJustNote)

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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Enemy

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg e | any isBlank (getModifiers e) && not (isBlanked msg) =
    runMessage (Blanked msg) e
  runMessage msg e = defaultRunMessage msg e

instance HasVictoryPoints Enemy where
  getVictoryPoints = enemyVictory . enemyAttrs

instance HasCount DoomCount () Enemy where
  getCount _ = DoomCount . enemyDoom . enemyAttrs

instance HasId LocationId () Enemy where
  getId _ = enemyLocation . enemyAttrs

instance HasCardCode Enemy where
  getCardCode = enemyCardCode . enemyAttrs

instance HasTraits Enemy where
  getTraits = enemyTraits . enemyAttrs

instance HasKeywords Enemy where
  getKeywords = enemyKeywords . enemyAttrs

instance HasModifiers Enemy where
  getModifiers = concat . toList . enemyModifiers . enemyAttrs

instance HasId EnemyId () Enemy where
  getId _ = enemyId . enemyAttrs

instance IsEnemy Enemy where
  isAloof = isAloof . enemyAttrs

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
  ]

isEngaged :: Enemy -> Bool
isEngaged = not . null . enemyEngagedInvestigators . enemyAttrs

isExhausted :: Enemy -> Bool
isExhausted = enemyExhausted . enemyAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . enemyAttrs

getBearer :: Enemy -> Maybe InvestigatorId
getBearer enemy = case enemyPrey (enemyAttrs enemy) of
  Bearer iid -> Just (InvestigatorId $ unBearerId iid)
  _ -> Nothing

isBlank :: Modifier -> Bool
isBlank Blank{} = True
isBlank _ = False

isBlanked :: Message -> Bool
isBlanked Blanked{} = True
isBlanked _ = False

enemyAttrs :: Enemy -> Attrs
enemyAttrs = getAttrs

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a
