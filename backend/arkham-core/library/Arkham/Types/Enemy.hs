{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy
  ( lookupEnemy
  , baseEnemy
  , isEngaged
  , isUnique
  , getEngagedInvestigators
  , getBearer
  , getEnemyVictory
  , Enemy
  )
where

import Arkham.Import

import Arkham.Types.Action
import Arkham.Types.Trait (Trait)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Cards
import Arkham.Types.Enemy.Runner
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
  | TheExperiment' TheExperiment
  | CloverClubPitBoss' CloverClubPitBoss
  | Thrall' Thrall
  | WizardOfYogSothoth' WizardOfYogSothoth
  | Whippoorwill' Whippoorwill
  | AvianThrall' AvianThrall
  | LupineThrall' LupineThrall
  | OBannionsThug' OBannionsThug
  | Mobster' Mobster
  | ConglomerationOfSpheres' ConglomerationOfSpheres
  | ServantOfTheLurker' ServantOfTheLurker
  | HuntingHorror' HuntingHorror
  | CorpseHungryGhoul' CorpseHungryGhoul
  | GhoulFromTheDepths' GhoulFromTheDepths
  | Narogath' Narogath
  | GraveEater' GraveEater
  | AcolyteOfUmordhoth' AcolyteOfUmordhoth
  | DiscipleOfTheDevourer' DiscipleOfTheDevourer
  | CorpseTaker' CorpseTaker
  | JeremiahPierce' JeremiahPierce
  | BillyCooper' BillyCooper
  | AlmaHill' AlmaHill
  | BogGator' BogGator
  | SwampLeech' SwampLeech
  | TheRougarou' TheRougarou
  | SlimeCoveredDhole' SlimeCoveredDhole
  | MarshGug' MarshGug
  | DarkYoungHost' DarkYoungHost
  | BaseEnemy' BaseEnemy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BaseEnemy = BaseEnemy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseEnemy :: EnemyId -> CardCode -> (Attrs -> Attrs) -> Enemy
baseEnemy a b f = BaseEnemy' . BaseEnemy $ baseAttrs a b f

instance ActionRunner env => HasActions env BaseEnemy where
  getActions investigator window (BaseEnemy attrs) =
    getActions investigator window attrs

instance HasModifiersFor env BaseEnemy where
  getModifiersFor = noModifiersFor

instance (EnemyRunner env) => RunMessage env BaseEnemy where
  runMessage msg (BaseEnemy attrs) = BaseEnemy <$> runMessage msg attrs

actionFromMessage :: Message -> Maybe Action
actionFromMessage (ActivateCardAbilityAction _ ability) =
  case abilityType ability of
    ActionAbility _ maction -> maction
    _ -> Nothing
actionFromMessage _ = Nothing

preventedByModifier :: Attrs -> Message -> Modifier -> Bool
preventedByModifier Attrs {..} msg (Modifier _ (CannotTakeAction matcher)) =
  case actionFromMessage msg of
    Just action -> case matcher of
      IsAction a -> a == action
      EnemyAction a traits ->
        a == action && not (null $ setFromList traits `intersect` enemyTraits)
      FirstOneOf _ -> False -- TODO: We can't tell here
    Nothing -> False
preventedByModifier _ _ _ = False

instance ActionRunner env => HasActions env Enemy where
  getActions investigator window enemy = do
    modifiers' <- getModifiersFor
      (toSource enemy)
      (InvestigatorTarget investigator)
      ()
    actions <- defaultGetActions investigator window enemy

    -- preventedByModifier :: Attrs -> Modifier -> Action -> Bool
    pure $ filter
      (\action ->
        not $ any (preventedByModifier (enemyAttrs enemy) action) modifiers'
      )
      actions

deriving anyclass instance
  ( HasId LocationId env InvestigatorId
  , HasCount RemainingSanity env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount PlayerCount env ()
  , HasSet InvestigatorId env LocationId
  , HasSet ConnectedLocationId env LocationId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor env Enemy

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  toId = toId . enemyAttrs
  toSource = toSource . enemyAttrs
  toTarget = toTarget . enemyAttrs
  isSource = isSource . enemyAttrs
  isTarget = isTarget . enemyAttrs

instance (EnemyRunner env) => RunMessage env Enemy where
  runMessage msg e = do
    modifiers' <- getModifiersFor (toSource e) (toTarget e) ()
    if any isBlank modifiers' && not (isBlanked msg)
      then runMessage (Blanked msg) e
      else defaultRunMessage msg e

instance HasVictoryPoints Enemy where
  getVictoryPoints = enemyVictory . enemyAttrs

instance HasCount DoomCount env Enemy where
  getCount = pure . DoomCount . enemyDoom . enemyAttrs

instance HasCount HealthDamageCount env Enemy where
  getCount = pure . HealthDamageCount . enemyHealthDamage . enemyAttrs

instance HasCount SanityDamageCount env Enemy where
  getCount = pure . SanityDamageCount . enemySanityDamage . enemyAttrs

instance HasId LocationId env Enemy where
  getId = pure . enemyLocation . enemyAttrs

instance HasSet TreacheryId env Enemy where
  getSet = pure . enemyTreacheries . enemyAttrs

instance HasSet AssetId env Enemy where
  getSet = pure . enemyAssets . enemyAttrs

instance IsCard Enemy where
  getCardId = getCardId . enemyAttrs
  getCardCode = getCardCode . enemyAttrs
  getTraits = getTraits . enemyAttrs
  getKeywords = getKeywords . enemyAttrs

instance IsEnemy Enemy where
  isAloof = isAloof . enemyAttrs

instance HasDamage Enemy where
  getDamage = (, 0) . enemyDamage . enemyAttrs

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

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
  , ("02058", TheExperiment' . theExperiment)
  , ("02078", CloverClubPitBoss' . cloverClubPitBoss)
  , ("02086", Thrall' . thrall)
  , ("02087", WizardOfYogSothoth' . wizardOfYogSothoth)
  , ("02090", Whippoorwill' . whippoorwill)
  , ("02094", AvianThrall' . avianThrall)
  , ("02095", LupineThrall' . lupineThrall)
  , ("02097", OBannionsThug' . oBannionsThug)
  , ("02098", Mobster' . mobster)
  , ("02103", ConglomerationOfSpheres' . conglomerationOfSpheres)
  , ("02104", ServantOfTheLurker' . servantOfTheLurker)
  , ("02141", HuntingHorror' . huntingHorror)
  , ("50022", CorpseHungryGhoul' . corpseHungryGhoul)
  , ("50023", GhoulFromTheDepths' . ghoulFromTheDepths)
  , ("50026b", Narogath' . narogath)
  , ("50038", GraveEater' . graveEater)
  , ("50039", AcolyteOfUmordhoth' . acolyteOfUmordhoth)
  , ("50041", DiscipleOfTheDevourer' . discipleOfTheDevourer)
  , ("50042", CorpseTaker' . corpseTaker)
  , ("50044", JeremiahPierce' . jeremiahPierce)
  , ("50045", BillyCooper' . billyCooper)
  , ("50046", AlmaHill' . almaHill)
  , ("81022", BogGator' . bogGator)
  , ("81023", SwampLeech' . swampLeech)
  , ("81028", TheRougarou' . theRougarou)
  , ("81031", SlimeCoveredDhole' . slimeCoveredDhole)
  , ("81032", MarshGug' . marshGug)
  , ("81033", DarkYoungHost' . darkYoungHost)
  , ("enemy", \eid -> baseEnemy eid "enemy" id)
  ]

isEngaged :: Enemy -> Bool
isEngaged = not . null . enemyEngagedInvestigators . enemyAttrs

isUnique :: Enemy -> Bool
isUnique = enemyUnique . enemyAttrs

instance Exhaustable Enemy where
  isExhausted = enemyExhausted . enemyAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . enemyAttrs

getEnemyVictory :: Enemy -> Maybe Int
getEnemyVictory = enemyVictory . enemyAttrs

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
  TheExperiment' attrs -> coerce attrs
  CloverClubPitBoss' attrs -> coerce attrs
  Thrall' attrs -> coerce attrs
  WizardOfYogSothoth' attrs -> coerce attrs
  Whippoorwill' attrs -> coerce attrs
  AvianThrall' attrs -> coerce attrs
  LupineThrall' attrs -> coerce attrs
  OBannionsThug' attrs -> coerce attrs
  Mobster' attrs -> coerce attrs
  ConglomerationOfSpheres' attrs -> coerce attrs
  ServantOfTheLurker' attrs -> coerce attrs
  HuntingHorror' attrs -> coerce attrs
  CorpseHungryGhoul' attrs -> coerce attrs
  GhoulFromTheDepths' attrs -> coerce attrs
  Narogath' attrs -> coerce attrs
  GraveEater' attrs -> coerce attrs
  AcolyteOfUmordhoth' attrs -> coerce attrs
  DiscipleOfTheDevourer' attrs -> coerce attrs
  CorpseTaker' attrs -> coerce attrs
  JeremiahPierce' attrs -> coerce attrs
  BillyCooper' attrs -> coerce attrs
  AlmaHill' attrs -> coerce attrs
  BogGator' attrs -> coerce attrs
  SwampLeech' attrs -> coerce attrs
  TheRougarou' (TheRougarou (attrs `With` _)) -> attrs
  SlimeCoveredDhole' attrs -> coerce attrs
  MarshGug' attrs -> coerce attrs
  DarkYoungHost' attrs -> coerce attrs
  BaseEnemy' attrs -> coerce attrs
