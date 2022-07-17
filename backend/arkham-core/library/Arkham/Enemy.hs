module Arkham.Enemy
  ( module Arkham.Enemy
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Enemies
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait (toTraits)
import Data.Typeable

data Enemy = forall a. IsEnemy a => Enemy a

instance Eq Enemy where
  (Enemy (a :: a)) == (Enemy (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Enemy where
  show (Enemy a) = show a

instance ToJSON Enemy where
  toJSON (Enemy a) = toJSON a

createEnemy :: (HasCallStack, IsCard a) => a -> Enemy
createEnemy a = lookupEnemy (toCardCode a) (EnemyId $ toCardId a)

actionFromMessage :: Ability -> Maybe Action
actionFromMessage ability = case abilityType ability of
  ActionAbility maction _ -> maction
  _ -> Nothing

preventedByModifier :: EnemyAttrs -> Ability -> ModifierType -> Bool
preventedByModifier e msg (CannotTakeAction matcher) =
  case actionFromMessage msg of
    Just action -> case matcher of
      IsAction a -> a == action
      EnemyAction a traits -> a == action && notNull
        (setFromList traits `intersect` toTraits (toCardDef e))
      FirstOneOf _ -> False -- TODO: We can't tell here
    Nothing -> False
preventedByModifier _ _ _ = False

instance HasAbilities Enemy where
  getAbilities (Enemy a) = getAbilities a

instance HasModifiersFor Enemy where
  getModifiersFor source target (Enemy a) = getModifiersFor source target a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance TargetEntity Enemy where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Enemy where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance RunMessage Enemy where
  runMessage msg e@(Enemy x) = do
    -- we must check that an enemy exists when grabbing modifiers
    -- as some messages are not masked when targetting cards in the
    -- discard.
    allEnemyIds <- select AnyEnemy
    modifiers' <- if toId e `member` allEnemyIds
      then getModifiers (toSource e) (toTarget e)
      else pure []
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    Enemy <$> runMessage msg' x

lookupEnemy :: HasCallStack => CardCode -> (EnemyId -> Enemy)
lookupEnemy cardCode =
  fromJustNote ("Unknown enemy: " <> pack (show cardCode))
    $ lookup cardCode allEnemies

instance FromJSON Enemy where
  parseJSON v = flip (withObject "Enemy") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      -- Night of the Zealot
      -- weakness
      "01101" -> Enemy . MobEnforcer <$> parseJSON v
      "01102" -> Enemy . SilverTwilightAcolyte <$> parseJSON v
      "01103" -> Enemy . StubbornDetective <$> parseJSON v
      -- The Gathering
      "01116" -> Enemy . GhoulPriest <$> parseJSON v
      "01118" -> Enemy . FleshEater <$> parseJSON v
      "01119" -> Enemy . IcyGhoul <$> parseJSON v
      -- The Midnight Masks
      "01121b" -> Enemy . TheMaskedHunter <$> parseJSON v
      "01137" -> Enemy . WolfManDrew <$> parseJSON v
      "01138" -> Enemy . HermanCollins <$> parseJSON v
      "01139" -> Enemy . PeterWarren <$> parseJSON v
      "01140" -> Enemy . VictoriaDevereux <$> parseJSON v
      "01141" -> Enemy . RuthTurner <$> parseJSON v
      -- The Devourer Below
      "01157" -> Enemy . Umordhoth <$> parseJSON v
      -- Rats
      "01159" -> Enemy . SwarmOfRats <$> parseJSON v
      -- Ghouls
      "01160" -> Enemy . GhoulMinion <$> parseJSON v
      "01161" -> Enemy . RavenousGhoul <$> parseJSON v
      -- Dark Cult
      "01169" -> Enemy . Acolyte <$> parseJSON v
      "01170" -> Enemy . WizardOfTheOrder <$> parseJSON v
      -- Nightgaunts
      "01172" -> Enemy . HuntingNightgaunt <$> parseJSON v
      -- Agents of Hastur
      "01175" -> Enemy . ScreechingByakhee <$> parseJSON v
      -- Agents of Yog-Sothoth
      "01177" -> Enemy . YithianObserver <$> parseJSON v
      -- Agents of Shub-Niggurath
      "01179" -> Enemy . RelentlessDarkYoung <$> parseJSON v
      "01180" -> Enemy . GoatSpawn <$> parseJSON v
      -- Agents of Cthulhu
      "01181" -> Enemy . YoungDeepOne <$> parseJSON v
      -- The Dunwich Legacy
      -- Extracurricular Activity
      "02058" -> Enemy . TheExperiment <$> parseJSON v
      -- The House Always Wins
      "02078" -> Enemy . CloverClubPitBoss <$> parseJSON v
      -- Bishop's Thralls
      "02086" -> Enemy . Thrall <$> parseJSON v
      "02087" -> Enemy . WizardOfYogSothoth <$> parseJSON v
      -- Whippoorwill
      "02090" -> Enemy . Whippoorwill <$> parseJSON v
      -- Beast Thralls
      "02094" -> Enemy . AvianThrall <$> parseJSON v
      "02095" -> Enemy . LupineThrall <$> parseJSON v
      -- Naomi's Crew
      "02097" -> Enemy . OBannionsThug <$> parseJSON v
      "02098" -> Enemy . Mobster <$> parseJSON v
      -- Hideous Abominations
      "02103" -> Enemy . ConglomerationOfSpheres <$> parseJSON v
      "02104" -> Enemy . ServantOfTheLurker <$> parseJSON v
      -- The Miskatonic Museum
      "02141" -> Enemy . HuntingHorror <$> parseJSON v
      -- The Essex County Express
      "02182" -> Enemy . GrapplingHorror <$> parseJSON v
      "02183" -> Enemy . EmergentMonstrosity <$> parseJSON v
      -- Blood on the Altar
      "02216" -> Enemy . SilasBishop <$> parseJSON v
      "02224" -> Enemy . ServantOfManyMouths <$> parseJSON v
      -- Undimensioned and Unseen
      "02255" -> Enemy . BroodOfYogSothoth <$> parseJSON v
      -- Where Doom Awaits
      "02293" -> Enemy . SethBishop <$> parseJSON v
      "02294" -> Enemy . DevoteeOfTheKey <$> parseJSON v
      "02295" -> Enemy . CrazedShoggoth <$> parseJSON v
      -- Lost in Time and Space
      "02323" -> Enemy . YogSothoth <$> parseJSON v
      "02329" -> Enemy . InterstellarTraveler <$> parseJSON v
      "02330" -> Enemy . YithianStarseeker <$> parseJSON v
      -- The Path to Carcosa
      -- signature
      "03017" -> Enemy . GraveyardGhouls <$> parseJSON v
      -- weakness
      "03042" -> Enemy . TheThingThatFollows <$> parseJSON v
      -- Curtain Call
      "03059" -> Enemy . TheManInThePallidMask <$> parseJSON v
      "03060" -> Enemy . RoyalEmissary <$> parseJSON v
      -- The Last King
      "03065b" -> Enemy . ConstanceDumaine <$> parseJSON v
      "03066b" -> Enemy . JordanPerry <$> parseJSON v
      "03067b" -> Enemy . IshimaruHaruko <$> parseJSON v
      "03068b" -> Enemy . SebastienMoreau <$> parseJSON v
      "03069b" -> Enemy . AshleighClarke <$> parseJSON v
      "03081" -> Enemy . DianneDevine <$> parseJSON v
      -- Byakhee
      "03086" -> Enemy . SwiftByakhee <$> parseJSON v
      -- Inhabitants of Carcosa
      "03088" -> Enemy . BeastOfAldebaran <$> parseJSON v
      "03089" -> Enemy . SpawnOfHali <$> parseJSON v
      -- Hauntings
      "03093" -> Enemy . Poltergeist <$> parseJSON v
      -- Hastur's Gift
      "03095" -> Enemy . Maniac <$> parseJSON v
      "03096" -> Enemy . YoungPsychopath <$> parseJSON v
      -- Cult of the Yellow Sign
      "03098" -> Enemy . Fanatic <$> parseJSON v
      "03099" -> Enemy . AgentOfTheKing <$> parseJSON v
      -- Decay and Filth
      "03103" -> Enemy . RoachSwarm <$> parseJSON v
      -- Echoes of the Past
      "03140" -> Enemy . PossessedOathspeaker <$> parseJSON v
      "03144" -> Enemy . SeekerOfCarcosa <$> parseJSON v
      -- The Unspeakable Oath
      "03182b" -> Enemy . DanielChesterfield <$> parseJSON v
      "03183" -> Enemy . AsylumGorger <$> parseJSON v
      "03184" -> Enemy . MadPatient <$> parseJSON v
      -- A Phantom of Truth
      "03221a" -> Enemy . TheOrganistHopelessIDefiedHim <$> parseJSON v
      "03221b" -> Enemy . TheOrganistDrapedInMystery <$> parseJSON v
      "03222" -> Enemy . StealthyByakhee <$> parseJSON v
      -- The Pallid Mask
      "03241b" -> Enemy . SpecterOfDeath <$> parseJSON v
      "03258" -> Enemy . CatacombsDocent <$> parseJSON v
      "03259" -> Enemy . CorpseDweller <$> parseJSON v
      -- Black Stars Rise
      "03300" -> Enemy . TidalTerror <$> parseJSON v
      "03301" -> Enemy . RiftSeeker <$> parseJSON v
      -- Dim Carcosa
      "03332" -> Enemy . HasturTheKingInYellow <$> parseJSON v
      "03333" -> Enemy . HasturLordOfCarcosa <$> parseJSON v
      "03334" -> Enemy . HasturTheTatteredKing <$> parseJSON v
      "03335" -> Enemy . CreatureOutOfDemhe <$> parseJSON v
      "03336" -> Enemy . WingedOne <$> parseJSON v
      -- The Forgotten Age
      -- signature
      "04014" -> Enemy . SerpentsOfYig <$> parseJSON v
      -- Return to Night of the Zealot
      -- Return to the Gathering
      "50022" -> Enemy . CorpseHungryGhoul <$> parseJSON v
      "50023" -> Enemy . GhoulFromTheDepths <$> parseJSON v
      -- Return to the Midnight Masks
      "50026b" -> Enemy . Narogath <$> parseJSON v
      -- Ghouls of Umordhoth
      "50038" -> Enemy . GraveEater <$> parseJSON v
      "50039" -> Enemy . AcolyteOfUmordhoth <$> parseJSON v
      -- The Devourer's Cult
      "50041" -> Enemy . DiscipleOfTheDevourer <$> parseJSON v
      "50042" -> Enemy . CorpseTaker <$> parseJSON v
      -- Return to Cult of Umordhoth
      "50044" -> Enemy . JeremiahPierce <$> parseJSON v
      "50045" -> Enemy . BillyCooper <$> parseJSON v
      "50046" -> Enemy . AlmaHill <$> parseJSON v
      -- Nathanial Cho
      "60103" -> Enemy . TommyMalloy <$> parseJSON v
      -- Curse of the Rougarou
      "81022" -> Enemy . BogGator <$> parseJSON v
      "81023" -> Enemy . SwampLeech <$> parseJSON v
      "81028" -> Enemy . TheRougarou <$> parseJSON v
      "81031" -> Enemy . SlimeCoveredDhole <$> parseJSON v
      "81032" -> Enemy . MarshGug <$> parseJSON v
      "81033" -> Enemy . DarkYoungHost <$> parseJSON v
      -- Carnevale of Horrors
      "82002b" -> Enemy . BalefulReveler <$> parseJSON v
      "82017" -> Enemy . DonLagorio <$> parseJSON v
      "82018" -> Enemy . ElisabettaMagro <$> parseJSON v
      "82019" -> Enemy . SalvatoreNeri <$> parseJSON v
      "82020" -> Enemy . SavioCorvi <$> parseJSON v
      "82027" -> Enemy . Cnidathqua <$> parseJSON v
      "82028" -> Enemy . Poleman <$> parseJSON v
      "82029" -> Enemy . CarnevaleSentinel <$> parseJSON v
      "82030" -> Enemy . WrithingAppendage <$> parseJSON v
      _ -> error "unknown enemy"

allEnemies :: HashMap CardCode (EnemyId -> Enemy)
allEnemies = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ -- Night of the Zealot
  -- weakness
    Enemy <$> mobEnforcer
  , Enemy <$> silverTwilightAcolyte
  , Enemy <$> stubbornDetective
  -- The Gathering
  , Enemy <$> ghoulPriest
  , Enemy <$> fleshEater
  , Enemy <$> icyGhoul
  -- The Midnight Masks
  , Enemy <$> theMaskedHunter
  , Enemy <$> wolfManDrew
  , Enemy <$> hermanCollins
  , Enemy <$> peterWarren
  , Enemy <$> victoriaDevereux
  , Enemy <$> ruthTurner
  -- The Devourer Below
  , Enemy <$> umordhoth
  -- Rats
  , Enemy <$> swarmOfRats
  -- Ghouls
  , Enemy <$> ghoulMinion
  , Enemy <$> ravenousGhoul
  -- Dark Cult
  , Enemy <$> acolyte
  , Enemy <$> wizardOfTheOrder
  -- Nightgaunts
  , Enemy <$> huntingNightgaunt
  -- Agents of Hastur
  , Enemy <$> screechingByakhee
  -- Agents of Yog-Sothoth
  , Enemy <$> yithianObserver
  -- Agents of Shub-Niggurath
  , Enemy <$> relentlessDarkYoung
  , Enemy <$> goatSpawn
  -- Agents of Cthulhu
  , Enemy <$> youngDeepOne
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , Enemy <$> theExperiment
  -- The House Always Wins
  , Enemy <$> cloverClubPitBoss
  -- Bishop's Thralls
  , Enemy <$> thrall
  , Enemy <$> wizardOfYogSothoth
  -- Whippoorwill
  , Enemy <$> whippoorwill
  -- Beast Thralls
  , Enemy <$> avianThrall
  , Enemy <$> lupineThrall
  -- Naomi's Crew
  , Enemy <$> oBannionsThug
  , Enemy <$> mobster
  -- Hideous Abominations
  , Enemy <$> conglomerationOfSpheres
  , Enemy <$> servantOfTheLurker
  -- The Miskatonic Museum
  , Enemy <$> huntingHorror
  -- The Essex County Express
  , Enemy <$> grapplingHorror
  , Enemy <$> emergentMonstrosity
  -- Blood on the Altar
  , Enemy <$> silasBishop
  , Enemy <$> servantOfManyMouths
  -- Undimensioned and Unseen
  , Enemy <$> broodOfYogSothoth
  -- Where Doom Awaits
  , Enemy <$> sethBishop
  , Enemy <$> devoteeOfTheKey
  , Enemy <$> crazedShoggoth
  -- Lost in Time and Space
  , Enemy <$> yogSothoth
  , Enemy <$> interstellarTraveler
  , Enemy <$> yithianStarseeker
  -- The Path to Carcosa
  -- signature
  , Enemy <$> graveyardGhouls
  -- weakness
  , Enemy <$> theThingThatFollows
  -- Curtain Call
  , Enemy <$> theManInThePallidMask
  , Enemy <$> royalEmissary
  -- The Last King
  , Enemy <$> constanceDumaine
  , Enemy <$> jordanPerry
  , Enemy <$> ishimaruHaruko
  , Enemy <$> sebastienMoreau
  , Enemy <$> ashleighClarke
  , Enemy <$> dianneDevine
  -- Byakhee
  , Enemy <$> swiftByakhee
  -- Inhabitants of Carcosa
  , Enemy <$> beastOfAldebaran
  , Enemy <$> spawnOfHali
  -- Hauntings
  , Enemy <$> poltergeist
  -- Hastur's Gift
  , Enemy <$> maniac
  , Enemy <$> youngPsychopath
  -- Cult of the Yellow Sign
  , Enemy <$> fanatic
  , Enemy <$> agentOfTheKing
  -- Decay and Filth
  , Enemy <$> roachSwarm
  -- Echoes of the Past
  , Enemy <$> possessedOathspeaker
  , Enemy <$> seekerOfCarcosa
  -- The Unspeakable Oath
  , Enemy <$> danielChesterfield
  , Enemy <$> asylumGorger
  , Enemy <$> madPatient
  -- A Phantom of Truth
  , Enemy <$> theOrganistHopelessIDefiedHim
  , Enemy <$> theOrganistDrapedInMystery
  , Enemy <$> stealthyByakhee
  -- The Pallid Mask
  , Enemy <$> specterOfDeath
  , Enemy <$> catacombsDocent
  , Enemy <$> corpseDweller
  -- Black Stars Rise
  , Enemy <$> tidalTerror
  , Enemy <$> riftSeeker
  -- Dim Carcosa
  , Enemy <$> hasturTheKingInYellow
  , Enemy <$> hasturLordOfCarcosa
  , Enemy <$> hasturTheTatteredKing
  , Enemy <$> creatureOutOfDemhe
  , Enemy <$> wingedOne
  -- The Forgotten Age
  -- signature
  , Enemy <$> serpentsOfYig
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , Enemy <$> corpseHungryGhoul
  , Enemy <$> ghoulFromTheDepths
  -- Return to the Midnight Masks
  , Enemy <$> narogath
  -- Ghouls of Umordhoth
  , Enemy <$> graveEater
  , Enemy <$> acolyteOfUmordhoth
  -- The Devourer's Cult
  , Enemy <$> discipleOfTheDevourer
  , Enemy <$> corpseTaker
  -- Return to Cult of Umordhoth
  , Enemy <$> jeremiahPierce
  , Enemy <$> billyCooper
  , Enemy <$> almaHill
  -- Nathanial Cho
  , Enemy <$> tommyMalloy
  -- Curse of the Rougarou
  , Enemy <$> bogGator
  , Enemy <$> swampLeech
  , Enemy <$> theRougarou
  , Enemy <$> slimeCoveredDhole
  , Enemy <$> marshGug
  , Enemy <$> darkYoungHost
  -- Carnevale of Horrors
  , Enemy <$> balefulReveler
  , Enemy <$> donLagorio
  , Enemy <$> elisabettaMagro
  , Enemy <$> salvatoreNeri
  , Enemy <$> savioCorvi
  , Enemy <$> cnidathqua
  , Enemy <$> poleman
  , Enemy <$> carnevaleSentinel
  , Enemy <$> writhingAppendage
  ]
