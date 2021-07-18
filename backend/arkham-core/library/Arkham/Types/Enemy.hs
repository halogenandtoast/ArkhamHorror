module Arkham.Types.Enemy
  ( module Arkham.Types.Enemy
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Cards
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait, toTraits)
import Arkham.Types.TreacheryId

createEnemy :: IsCard a => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

data Enemy
  = BaseEnemy' BaseEnemy
  | Acolyte' Acolyte
  | AcolyteOfUmordhoth' AcolyteOfUmordhoth
  | AlmaHill' AlmaHill
  | AvianThrall' AvianThrall
  | BalefulReveler' BalefulReveler
  | BillyCooper' BillyCooper
  | BogGator' BogGator
  | BroodOfYogSothoth' BroodOfYogSothoth
  | CloverClubPitBoss' CloverClubPitBoss
  | ConglomerationOfSpheres' ConglomerationOfSpheres
  | CorpseHungryGhoul' CorpseHungryGhoul
  | CorpseTaker' CorpseTaker
  | CrazedShoggoth' CrazedShoggoth
  | DarkYoungHost' DarkYoungHost
  | DevoteeOfTheKey' DevoteeOfTheKey
  | DiscipleOfTheDevourer' DiscipleOfTheDevourer
  | EmergentMonstrosity' EmergentMonstrosity
  | FleshEater' FleshEater
  | GhoulFromTheDepths' GhoulFromTheDepths
  | GhoulMinion' GhoulMinion
  | GhoulPriest' GhoulPriest
  | GoatSpawn' GoatSpawn
  | GrapplingHorror' GrapplingHorror
  | GraveEater' GraveEater
  | HermanCollins' HermanCollins
  | HuntingHorror' HuntingHorror
  | HuntingNightgaunt' HuntingNightgaunt
  | IcyGhoul' IcyGhoul
  | InterstellarTraveler' InterstellarTraveler
  | JeremiahPierce' JeremiahPierce
  | LupineThrall' LupineThrall
  | MarshGug' MarshGug
  | MobEnforcer' MobEnforcer
  | Mobster' Mobster
  | Narogath' Narogath
  | OBannionsThug' OBannionsThug
  | PeterWarren' PeterWarren
  | Poleman' Poleman
  | RavenousGhoul' RavenousGhoul
  | RelentlessDarkYoung' RelentlessDarkYoung
  | RuthTurner' RuthTurner
  | ScreechingByakhee' ScreechingByakhee
  | ServantOfManyMouths' ServantOfManyMouths
  | ServantOfTheLurker' ServantOfTheLurker
  | SethBishop' SethBishop
  | SilasBishop' SilasBishop
  | SilverTwilightAcolyte' SilverTwilightAcolyte
  | SlimeCoveredDhole' SlimeCoveredDhole
  | StubbornDetective' StubbornDetective
  | SwampLeech' SwampLeech
  | SwarmOfRats' SwarmOfRats
  | TheExperiment' TheExperiment
  | TheMaskedHunter' TheMaskedHunter
  | TheRougarou' TheRougarou
  | Thrall' Thrall
  | Umordhoth' Umordhoth
  | VictoriaDevereux' VictoriaDevereux
  | Whippoorwill' Whippoorwill
  | WizardOfTheOrder' WizardOfTheOrder
  | WizardOfYogSothoth' WizardOfYogSothoth
  | WolfManDrew' WolfManDrew
  | YithianObserver' YithianObserver
  | YithianStarseeker' YithianStarseeker
  | YogSothoth' YogSothoth
  | YoungDeepOne' YoungDeepOne
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BaseEnemy = BaseEnemy EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasCardDef Enemy where
  toCardDef = toCardDef . toAttrs

baseEnemy
  :: EnemyId
  -> CardCode
  -> (EnemyAttrs -> EnemyAttrs)
  -> (CardDef -> CardDef)
  -> Enemy
baseEnemy eid cardCode attrsF defF = BaseEnemy' $ cbCardBuilder
  (enemyWith
    BaseEnemy
    (defF $ testCardDef EnemyType cardCode)
    (1, Static 1, 1)
    (0, 0)
    attrsF
  )
  eid

instance ActionRunner env => HasActions env BaseEnemy where
  getActions investigator window (BaseEnemy attrs) =
    getActions investigator window attrs

instance HasModifiersFor env BaseEnemy

instance (EnemyRunner env) => RunMessage env BaseEnemy where
  runMessage msg (BaseEnemy attrs) = BaseEnemy <$> runMessage msg attrs

actionFromMessage :: Message -> Maybe Action
actionFromMessage (UseAbility _ ability) = case abilityType ability of
  ActionAbility maction _ -> maction
  _ -> Nothing
actionFromMessage _ = Nothing

preventedByModifier :: EnemyAttrs -> Message -> Modifier -> Bool
preventedByModifier EnemyAttrs {..} msg (Modifier _ (CannotTakeAction matcher))
  = case actionFromMessage msg of
    Just action -> case matcher of
      IsAction a -> a == action
      EnemyAction a traits -> a == action && notNull
        (setFromList traits `intersect` toTraits enemyCardDef)
      FirstOneOf _ -> False -- TODO: We can't tell here
    Nothing -> False
preventedByModifier _ _ _ = False

instance ActionRunner env => HasActions env Enemy where
  getActions investigator window x = do
    modifiers' <- getModifiersFor
      (toSource x)
      (InvestigatorTarget investigator)
      ()
    actions <- defaultGetActions investigator window x
    pure $ filter
      (\action -> not $ any (preventedByModifier (toAttrs x) action) modifiers')
      actions

instance
  ( HasId LocationId env InvestigatorId
  , HasCount RemainingSanity env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount PlayerCount env ()
  , HasSet InvestigatorId env LocationId
  , HasSet ConnectedLocationId env LocationId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor env Enemy where
  getModifiersFor = genericGetModifiersFor

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs

instance Named Enemy where
  toName = toName . toAttrs

instance TargetEntity Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance
  ( EnemyRunner env
  , HasName env AssetId
  , HasSet ClosestLocationId env (LocationId, [Trait])
  , HasCount DiscardCount env InvestigatorId
  )
  => RunMessage env Enemy where
  runMessage msg e = do
    modifiers' <- getModifiersFor (toSource e) (toTarget e) ()
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' e

instance HasVictoryPoints Enemy where
  getVictoryPoints = getEnemyVictory

instance HasCount DoomCount env Enemy where
  getCount = pure . DoomCount . enemyDoom . toAttrs

instance HasCount ClueCount env Enemy where
  getCount = pure . ClueCount . enemyClues . toAttrs

instance HasCount FightCount env Enemy where
  getCount = pure . FightCount . enemyFight . toAttrs

instance HasCount HealthDamageCount env Enemy where
  getCount = pure . HealthDamageCount . enemyHealthDamage . toAttrs

instance HasCount SanityDamageCount env Enemy where
  getCount = pure . SanityDamageCount . enemySanityDamage . toAttrs

instance HasId LocationId env Enemy where
  getId = pure . enemyLocation . toAttrs

instance HasSet TreacheryId env Enemy where
  getSet = pure . enemyTreacheries . toAttrs

instance HasSet AssetId env Enemy where
  getSet = pure . enemyAssets . toAttrs

instance IsCard Enemy where
  toCardId = toCardId . toAttrs

instance HasDamage Enemy where
  getDamage = (, 0) . enemyDamage . toAttrs

lookupEnemy :: CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ CardBuilder
    { cbCardCode = "enemy"
    , cbCardBuilder = \eid -> baseEnemy eid "enemy" id id
    }
  , Acolyte' <$> acolyte
  , AcolyteOfUmordhoth' <$> acolyteOfUmordhoth
  , AlmaHill' <$> almaHill
  , AvianThrall' <$> avianThrall
  , BalefulReveler' <$> balefulReveler
  , BillyCooper' <$> billyCooper
  , BogGator' <$> bogGator
  , BroodOfYogSothoth' <$> broodOfYogSothoth
  , CloverClubPitBoss' <$> cloverClubPitBoss
  , ConglomerationOfSpheres' <$> conglomerationOfSpheres
  , CorpseHungryGhoul' <$> corpseHungryGhoul
  , CorpseTaker' <$> corpseTaker
  , CrazedShoggoth' <$> crazedShoggoth
  , DarkYoungHost' <$> darkYoungHost
  , DevoteeOfTheKey' <$> devoteeOfTheKey
  , DiscipleOfTheDevourer' <$> discipleOfTheDevourer
  , EmergentMonstrosity' <$> emergentMonstrosity
  , FleshEater' <$> fleshEater
  , GhoulFromTheDepths' <$> ghoulFromTheDepths
  , GhoulMinion' <$> ghoulMinion
  , GhoulPriest' <$> ghoulPriest
  , GoatSpawn' <$> goatSpawn
  , GrapplingHorror' <$> grapplingHorror
  , GraveEater' <$> graveEater
  , HermanCollins' <$> hermanCollins
  , HuntingHorror' <$> huntingHorror
  , HuntingNightgaunt' <$> huntingNightgaunt
  , IcyGhoul' <$> icyGhoul
  , InterstellarTraveler' <$> interstellarTraveler
  , JeremiahPierce' <$> jeremiahPierce
  , LupineThrall' <$> lupineThrall
  , MarshGug' <$> marshGug
  , MobEnforcer' <$> mobEnforcer
  , Mobster' <$> mobster
  , Narogath' <$> narogath
  , OBannionsThug' <$> oBannionsThug
  , PeterWarren' <$> peterWarren
  , Poleman' <$> poleman
  , RavenousGhoul' <$> ravenousGhoul
  , RelentlessDarkYoung' <$> relentlessDarkYoung
  , RuthTurner' <$> ruthTurner
  , ScreechingByakhee' <$> screechingByakhee
  , ServantOfManyMouths' <$> servantOfManyMouths
  , ServantOfTheLurker' <$> servantOfTheLurker
  , SethBishop' <$> sethBishop
  , SilasBishop' <$> silasBishop
  , SilverTwilightAcolyte' <$> silverTwilightAcolyte
  , SlimeCoveredDhole' <$> slimeCoveredDhole
  , StubbornDetective' <$> stubbornDetective
  , SwampLeech' <$> swampLeech
  , SwarmOfRats' <$> swarmOfRats
  , TheExperiment' <$> theExperiment
  , TheMaskedHunter' <$> theMaskedHunter
  , TheRougarou' <$> theRougarou
  , Thrall' <$> thrall
  , Umordhoth' <$> umordhoth
  , VictoriaDevereux' <$> victoriaDevereux
  , Whippoorwill' <$> whippoorwill
  , WizardOfTheOrder' <$> wizardOfTheOrder
  , WizardOfYogSothoth' <$> wizardOfYogSothoth
  , WolfManDrew' <$> wolfManDrew
  , YithianObserver' <$> yithianObserver
  , YithianStarseeker' <$> yithianStarseeker
  , YogSothoth' <$> yogSothoth
  , YoungDeepOne' <$> youngDeepOne
  ]

isEngaged :: Enemy -> Bool
isEngaged = notNull . enemyEngagedInvestigators . toAttrs

isUnique :: Enemy -> Bool
isUnique = cdUnique . toCardDef

instance Exhaustable Enemy where
  isExhausted = enemyExhausted . toAttrs

getEngagedInvestigators :: Enemy -> HashSet InvestigatorId
getEngagedInvestigators = enemyEngagedInvestigators . toAttrs

getEnemyVictory :: Enemy -> Maybe Int
getEnemyVictory = cdVictoryPoints . toCardDef

getBearer :: Enemy -> Maybe InvestigatorId
getBearer x = case enemyPrey (toAttrs x) of
  Bearer iid -> Just (unBearerId iid)
  _ -> Nothing
