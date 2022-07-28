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
lookupEnemy cardCode = case lookup cardCode allEnemies of
  Nothing -> error $ "Unknown enemy: " <> show cardCode
  Just (SomeEnemyCard a) -> Enemy <$> cbCardBuilder a

instance FromJSON Enemy where
  parseJSON v = flip (withObject "Enemy") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withEnemyCardCode cCode $ \(_ :: EnemyCard a) -> Enemy <$> parseJSON @a v

withEnemyCardCode
  :: CardCode
  -> (forall a. IsEnemy a => EnemyCard a -> r)
  -> r
withEnemyCardCode cCode f =
  case lookup cCode allEnemies of
    Nothing -> error $ "Unknown enemy: " <> show cCode
    Just (SomeEnemyCard a) -> f a

data SomeEnemyCard = forall a. IsEnemy a => SomeEnemyCard (EnemyCard a)

liftSomeEnemyCard :: (forall a. EnemyCard a -> b) -> SomeEnemyCard -> b
liftSomeEnemyCard f (SomeEnemyCard a) = f a

someEnemyCardCode :: SomeEnemyCard -> CardCode
someEnemyCardCode = liftSomeEnemyCard cbCardCode

allEnemies :: HashMap CardCode SomeEnemyCard
allEnemies = mapFromList $ map
  (toFst someEnemyCardCode)
  [ -- Night of the Zealot
  -- weakness
    SomeEnemyCard mobEnforcer
  , SomeEnemyCard silverTwilightAcolyte
  , SomeEnemyCard stubbornDetective
  -- The Gathering
  , SomeEnemyCard ghoulPriest
  , SomeEnemyCard fleshEater
  , SomeEnemyCard icyGhoul
  -- The Midnight Masks
  , SomeEnemyCard theMaskedHunter
  , SomeEnemyCard wolfManDrew
  , SomeEnemyCard hermanCollins
  , SomeEnemyCard peterWarren
  , SomeEnemyCard victoriaDevereux
  , SomeEnemyCard ruthTurner
  -- The Devourer Below
  , SomeEnemyCard umordhoth
  -- Rats
  , SomeEnemyCard swarmOfRats
  -- Ghouls
  , SomeEnemyCard ghoulMinion
  , SomeEnemyCard ravenousGhoul
  -- Dark Cult
  , SomeEnemyCard acolyte
  , SomeEnemyCard wizardOfTheOrder
  -- Nightgaunts
  , SomeEnemyCard huntingNightgaunt
  -- Agents of Hastur
  , SomeEnemyCard screechingByakhee
  -- Agents of Yog-Sothoth
  , SomeEnemyCard yithianObserver
  -- Agents of Shub-Niggurath
  , SomeEnemyCard relentlessDarkYoung
  , SomeEnemyCard goatSpawn
  -- Agents of Cthulhu
  , SomeEnemyCard youngDeepOne
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , SomeEnemyCard theExperiment
  -- The House Always Wins
  , SomeEnemyCard cloverClubPitBoss
  -- Bishop's Thralls
  , SomeEnemyCard thrall
  , SomeEnemyCard wizardOfYogSothoth
  -- Whippoorwill
  , SomeEnemyCard whippoorwill
  -- Beast Thralls
  , SomeEnemyCard avianThrall
  , SomeEnemyCard lupineThrall
  -- Naomi's Crew
  , SomeEnemyCard oBannionsThug
  , SomeEnemyCard mobster
  -- Hideous Abominations
  , SomeEnemyCard conglomerationOfSpheres
  , SomeEnemyCard servantOfTheLurker
  -- The Miskatonic Museum
  , SomeEnemyCard huntingHorror
  -- The Essex County Express
  , SomeEnemyCard grapplingHorror
  , SomeEnemyCard emergentMonstrosity
  -- Blood on the Altar
  , SomeEnemyCard silasBishop
  , SomeEnemyCard servantOfManyMouths
  -- Undimensioned and Unseen
  , SomeEnemyCard broodOfYogSothoth
  -- Where Doom Awaits
  , SomeEnemyCard sethBishop
  , SomeEnemyCard devoteeOfTheKey
  , SomeEnemyCard crazedShoggoth
  -- Lost in Time and Space
  , SomeEnemyCard yogSothoth
  , SomeEnemyCard interstellarTraveler
  , SomeEnemyCard yithianStarseeker
  -- The Path to Carcosa
  -- signature
  , SomeEnemyCard graveyardGhouls
  -- weakness
  , SomeEnemyCard theThingThatFollows
  -- Curtain Call
  , SomeEnemyCard theManInThePallidMask
  , SomeEnemyCard royalEmissary
  -- The Last King
  , SomeEnemyCard constanceDumaine
  , SomeEnemyCard jordanPerry
  , SomeEnemyCard ishimaruHaruko
  , SomeEnemyCard sebastienMoreau
  , SomeEnemyCard ashleighClarke
  , SomeEnemyCard dianneDevine
  -- Byakhee
  , SomeEnemyCard swiftByakhee
  -- Inhabitants of Carcosa
  , SomeEnemyCard beastOfAldebaran
  , SomeEnemyCard spawnOfHali
  -- Hauntings
  , SomeEnemyCard poltergeist
  -- Hastur's Gift
  , SomeEnemyCard maniac
  , SomeEnemyCard youngPsychopath
  -- Cult of the Yellow Sign
  , SomeEnemyCard fanatic
  , SomeEnemyCard agentOfTheKing
  -- Decay and Filth
  , SomeEnemyCard roachSwarm
  -- Echoes of the Past
  , SomeEnemyCard possessedOathspeaker
  , SomeEnemyCard seekerOfCarcosa
  -- The Unspeakable Oath
  , SomeEnemyCard danielChesterfield
  , SomeEnemyCard asylumGorger
  , SomeEnemyCard madPatient
  -- A Phantom of Truth
  , SomeEnemyCard theOrganistHopelessIDefiedHim
  , SomeEnemyCard theOrganistDrapedInMystery
  , SomeEnemyCard stealthyByakhee
  -- The Pallid Mask
  , SomeEnemyCard specterOfDeath
  , SomeEnemyCard catacombsDocent
  , SomeEnemyCard corpseDweller
  -- Black Stars Rise
  , SomeEnemyCard tidalTerror
  , SomeEnemyCard riftSeeker
  -- Dim Carcosa
  , SomeEnemyCard hasturTheKingInYellow
  , SomeEnemyCard hasturLordOfCarcosa
  , SomeEnemyCard hasturTheTatteredKing
  , SomeEnemyCard creatureOutOfDemhe
  , SomeEnemyCard wingedOne
  -- The Forgotten Age
  -- signature
  , SomeEnemyCard serpentsOfYig
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , SomeEnemyCard corpseHungryGhoul
  , SomeEnemyCard ghoulFromTheDepths
  -- Return to the Midnight Masks
  , SomeEnemyCard narogath
  -- Ghouls of Umordhoth
  , SomeEnemyCard graveEater
  , SomeEnemyCard acolyteOfUmordhoth
  -- The Devourer's Cult
  , SomeEnemyCard discipleOfTheDevourer
  , SomeEnemyCard corpseTaker
  -- Return to Cult of Umordhoth
  , SomeEnemyCard jeremiahPierce
  , SomeEnemyCard billyCooper
  , SomeEnemyCard almaHill
  -- Nathanial Cho
  , SomeEnemyCard tommyMalloy
  -- Curse of the Rougarou
  , SomeEnemyCard bogGator
  , SomeEnemyCard swampLeech
  , SomeEnemyCard theRougarou
  , SomeEnemyCard slimeCoveredDhole
  , SomeEnemyCard marshGug
  , SomeEnemyCard darkYoungHost
  -- Carnevale of Horrors
  , SomeEnemyCard balefulReveler
  , SomeEnemyCard donLagorio
  , SomeEnemyCard elisabettaMagro
  , SomeEnemyCard salvatoreNeri
  , SomeEnemyCard savioCorvi
  , SomeEnemyCard cnidathqua
  , SomeEnemyCard poleman
  , SomeEnemyCard carnevaleSentinel
  , SomeEnemyCard writhingAppendage
  ]
