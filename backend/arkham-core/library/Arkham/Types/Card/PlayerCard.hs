module Arkham.Types.Card.PlayerCard where

import Arkham.Json
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.FastWindow
import Arkham.Types.Keyword
import Arkham.Types.SkillType
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  | PlayerEnemyType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BearerId = BearerId { unBearerId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

data PlayerCard = MkPlayerCard
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: Int
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcBearer :: Maybe BearerId
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: [Trait]
  , pcKeywords :: [Keyword]
  , pcFast :: Bool
  , pcFastWindows :: HashSet FastWindow
  , pcId :: CardId
  , pcAction :: Maybe Action
  , pcRevelation :: Bool
  , pcVictoryPoints :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasCardCode PlayerCard where
  getCardCode = pcCardCode

instance HasCardId PlayerCard where
  getCardId = pcId

instance HasCost PlayerCard where
  getCost = pcCost

lookupPlayerCard :: CardCode -> (CardId -> PlayerCard)
lookupPlayerCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ HashMap.lookup cardCode allPlayerCards

basePlayerCard
  :: CardId
  -> CardCode
  -> Text
  -> Int
  -> PlayerCardType
  -> ClassSymbol
  -> PlayerCard
basePlayerCard cardId cardCode name cost cardType classSymbol = MkPlayerCard
  { pcCardCode = cardCode
  , pcName = name
  , pcCost = cost
  , pcLevel = 0
  , pcCardType = cardType
  , pcWeakness = False
  , pcBearer = Nothing
  , pcClassSymbol = classSymbol
  , pcSkills = mempty
  , pcTraits = mempty
  , pcKeywords = mempty
  , pcFast = False
  , pcFastWindows = mempty
  , pcId = cardId
  , pcAction = Nothing
  , pcRevelation = False
  , pcVictoryPoints = Nothing
  }

asset :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> PlayerCard
asset cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost AssetType classSymbol

event :: CardId -> CardCode -> Text -> Int -> ClassSymbol -> PlayerCard
event cardId cardCode name cost classSymbol =
  basePlayerCard cardId cardCode name cost EventType classSymbol

skill :: CardId -> CardCode -> Text -> [SkillType] -> ClassSymbol -> PlayerCard
skill cardId cardCode name skills classSymbol =
  (basePlayerCard cardId cardCode name 0 SkillType classSymbol)
    { pcSkills = skills
    }

treachery :: CardId -> CardCode -> Text -> Int -> PlayerCard
treachery cardId cardCode name cost =
  (basePlayerCard cardId cardCode name cost PlayerTreacheryType Neutral)
    { pcWeakness = True
    }

enemy :: CardId -> CardCode -> Text -> Int -> PlayerCard
enemy cardId cardCode name cost =
  (basePlayerCard cardId cardCode name cost PlayerEnemyType Neutral)
    { pcWeakness = True
    }

playerCardMatch :: (PlayerCardType, Maybe Trait) -> PlayerCard -> Bool
playerCardMatch (cardType, mtrait) MkPlayerCard {..} =
  pcCardType == cardType && maybe True (`elem` pcTraits) mtrait

allPlayerCards :: HashMap CardCode (CardId -> PlayerCard)
allPlayerCards = HashMap.fromList
  [ ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01008", daisysToteBag)
  , ("01009", theNecronomicon)
  , ("01010", onTheLam)
  , ("01012", heirloomOfHyperborea)
  , ("01013", darkMemory)
  , ("01014", wendysAmulet)
  , ("01015", abandonedAndAlone)
  , ("01016", fortyFiveAutomatic)
  , ("01017", physicalTraining)
  , ("01018", beatCop)
  , ("01019", firstAid)
  , ("01020", machete)
  , ("01021", guardDog)
  , ("01022", evidence)
  , ("01023", dodge)
  , ("01024", dynamiteBlast)
  , ("01025", viciousBlow)
  , ("01030", magnifyingGlass)
  , ("01031", oldBookOfLore)
  , ("01032", researchLibrarian)
  , ("01033", drMilanChristopher)
  , ("01034", hyperawareness)
  , ("01035", medicalTexts)
  , ("01036", mindOverMatter)
  , ("01037", workingAHunch)
  , ("01038", barricade)
  , ("01039", deduction)
  , ("01044", switchblade)
  , ("01045", burglary)
  , ("01046", pickpoketing)
  , ("01047", fortyOneDerringer)
  , ("01048", leoDeLuca)
  , ("01049", hardKnocks)
  , ("01050", elusive)
  , ("01051", backstab)
  , ("01052", sneakAttack)
  , ("01058", forbiddenKnowledge)
  , ("01059", holyRosary)
  , ("01060", shrivelling)
  , ("01061", scrying)
  , ("01062", arcaneStudies)
  , ("01063", arcaneInitiate)
  , ("01064", drawnToTheFlame)
  , ("01065", wardOfProtection)
  , ("01066", blindingLight)
  , ("01067", fearless)
  , ("01072", leatherCoat)
  , ("01073", scavenging)
  , ("01074", baseballBat)
  , ("01075", rabbitsFoot)
  , ("01076", strayCat)
  , ("01077", digDeep)
  , ("01078", cunningDistraction)
  , ("01080", lucky)
  , ("01086", knife)
  , ("01087", flashlight)
  , ("01088", emergencyCache)
  , ("01089", guts)
  , ("01090", perception)
  , ("01091", overpower)
  , ("01092", manualDexterity)
  , ("01093", unexpectedCourage)
  , ("01096", amnesia)
  , ("01097", paranoia)
  , ("01099", psychosis)
  , ("01100", hypochondria)
  , ("01102", silverTwilightAcolyte)
  , ("01117", litaChantler)
  ]

rolands38Special :: CardId -> PlayerCard
rolands38Special cardId =
  (asset cardId "01006" "Roland's .38 Special" 3 Neutral)
    { pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = [Item, Weapon, Firearm]
    }

coverUp :: CardId -> PlayerCard
coverUp cardId = (treachery cardId "01007" "Cover Up" 0)
  { pcTraits = [Task]
  , pcRevelation = True
  }

daisysToteBag :: CardId -> PlayerCard
daisysToteBag cardId = (asset cardId "01008" "Daisy's Tote Bag" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = [Item]
  }

theNecronomicon :: CardId -> PlayerCard
theNecronomicon cardId = (asset cardId "01009" "The Necronomicon" 0 Neutral)
  { pcTraits = [Item, Tome]
  , pcWeakness = True
  , pcRevelation = True
  }

onTheLam :: CardId -> PlayerCard
onTheLam cardId = (asset cardId "01010" "On the Lam" 1 Neutral)
  { pcTraits = [Tactic]
  , pcSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [AfterTurnBegins You]
  }

heirloomOfHyperborea :: CardId -> PlayerCard
heirloomOfHyperborea cardId =
  (asset cardId "01012" "Heirloom of Hyperborea" 3 Neutral)
    { pcSkills = [SkillWillpower, SkillCombat, SkillWild]
    , pcTraits = [Item, Relic]
    }

darkMemory :: CardId -> PlayerCard
darkMemory cardId =
  (event cardId "01013" "Dark Memory" 2 Neutral) { pcTraits = [Spell] }

wendysAmulet :: CardId -> PlayerCard
wendysAmulet cardId = (asset cardId "01014" "Wendy's Amulet" 2 Neutral)
  { pcSkills = [SkillWild, SkillWild]
  , pcTraits = [Item, Relic]
  }

abandonedAndAlone :: CardId -> PlayerCard
abandonedAndAlone cardId = (treachery cardId "01015" "Abandoned and Alone" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

fortyFiveAutomatic :: CardId -> PlayerCard
fortyFiveAutomatic cardId = (asset cardId "01016" ".45 Automatic" 4 Guardian)
  { pcSkills = [SkillAgility]
  , pcTraits = [Item, Weapon, Firearm]
  }

physicalTraining :: CardId -> PlayerCard
physicalTraining cardId = (asset cardId "01017" "Physical Training" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = [Talent]
  }

beatCop :: CardId -> PlayerCard
beatCop cardId = (asset cardId "01018" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Ally, Police]
  }

firstAid :: CardId -> PlayerCard
firstAid cardId = (asset cardId "01019" "First Aid" 2 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Talent, Science]
  }

machete :: CardId -> PlayerCard
machete cardId = (asset cardId "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }

guardDog :: CardId -> PlayerCard
guardDog cardId = (asset cardId "01021" "Guard Dog" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = [Ally, Creature]
  }

evidence :: CardId -> PlayerCard
evidence cardId = (event cardId "01022" "Evidence!" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = [Insight]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [WhenEnemyDefeated You]
  }

dodge :: CardId -> PlayerCard
dodge cardId = (event cardId "01023" "Dodge" 1 Guardian)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = [Tactic]
  , pcFast = True
  , pcFastWindows = HashSet.fromList
    [WhenEnemyAttacks InvestigatorAtYourLocation]
  }

dynamiteBlast :: CardId -> PlayerCard
dynamiteBlast cardId = (event cardId "01024" "Dynamite Blast" 5 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Tactic]
  }

viciousBlow :: CardId -> PlayerCard
viciousBlow cardId =
  (skill cardId "01025" "Vicious Blow" [SkillCombat] Guardian)
    { pcTraits = [Practiced]
    }

magnifyingGlass :: CardId -> PlayerCard
magnifyingGlass cardId = (asset cardId "01030" "Magnifying Glass" 1 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Item, Tool]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

oldBookOfLore :: CardId -> PlayerCard
oldBookOfLore cardId = (asset cardId "01031" "Old Book of Lore" 3 Seeker)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Item, Tome]
  }

researchLibrarian :: CardId -> PlayerCard
researchLibrarian cardId = (asset cardId "01032" "Research Librarian" 2 Seeker)
  { pcSkills = [SkillAgility]
  , pcTraits = [Ally, Miskatonic]
  }

drMilanChristopher :: CardId -> PlayerCard
drMilanChristopher cardId =
  (asset cardId "01033" "Dr. Milan Christopher" 4 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Ally, Miskatonic]
    }

hyperawareness :: CardId -> PlayerCard
hyperawareness cardId = (asset cardId "01034" "Hyperawareness" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Talent]
  }

medicalTexts :: CardId -> PlayerCard
medicalTexts cardId = (asset cardId "01035" "Medical Texts" 2 Seeker)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Tome]
  }

mindOverMatter :: CardId -> PlayerCard
mindOverMatter cardId = (event cardId "01036" "Mind over Matter" 1 Seeker)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Insight]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

workingAHunch :: CardId -> PlayerCard
workingAHunch cardId = (event cardId "01037" "Working a Hunch" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = [Insight]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

barricade :: CardId -> PlayerCard
barricade cardId = (event cardId "01038" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = [Insight, Tactic]
  }

deduction :: CardId -> PlayerCard
deduction cardId = (skill cardId "01039" "Deduction" [SkillIntellect] Seeker)
  { pcTraits = [Practiced]
  }

switchblade :: CardId -> PlayerCard
switchblade cardId = (asset cardId "01044" "Switchbalde" 1 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = [Item, Weapon, Melee, Illicit]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

burglary :: CardId -> PlayerCard
burglary cardId = (asset cardId "01045" "Burglary" 1 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Talent, Illicit]
  }

pickpoketing :: CardId -> PlayerCard
pickpoketing cardId = (asset cardId "01046" "Pickpocketing" 2 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = [Talent, Illicit]
  }

fortyOneDerringer :: CardId -> PlayerCard
fortyOneDerringer cardId = (asset cardId "01047" ".41 Derringer" 3 Rogue)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Firearm, Illicit]
  }

leoDeLuca :: CardId -> PlayerCard
leoDeLuca cardId = (asset cardId "01048" "Leo De Luca" 6 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Ally, Criminal]
  }

hardKnocks :: CardId -> PlayerCard
hardKnocks cardId = (asset cardId "01049" "Hard Knocks" 2 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Talent]
  }

elusive :: CardId -> PlayerCard
elusive cardId = (event cardId "01050" "Elusive" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Tactic]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [DuringTurn You]
  }

backstab :: CardId -> PlayerCard
backstab cardId = (event cardId "01051" "Backstab" 3 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = [Tactic]
  , pcAction = Just Action.Fight
  }

sneakAttack :: CardId -> PlayerCard
sneakAttack cardId = (event cardId "01052" "Sneak Attack" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = [Tactic]
  }

forbiddenKnowledge :: CardId -> PlayerCard
forbiddenKnowledge cardId =
  (asset cardId "01058" "Forbidden Knowledge" 0 Mystic)
    { pcSkills = [SkillIntellect]
    , pcTraits = [Talent]
    }

holyRosary :: CardId -> PlayerCard
holyRosary cardId = (asset cardId "01059" "Holy Rosary" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Item, Charm]
  }

shrivelling :: CardId -> PlayerCard
shrivelling cardId = (asset cardId "01060" "Shrivelling" 3 Mystic)
  { pcSkills = [SkillCombat]
  , pcTraits = [Spell]
  }

scrying :: CardId -> PlayerCard
scrying cardId = (asset cardId "01061" "Scrying" 1 Mystic)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Spell]
  }

arcaneStudies :: CardId -> PlayerCard
arcaneStudies cardId = (asset cardId "01062" "Arcane Studies" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = [Talent]
  }

arcaneInitiate :: CardId -> PlayerCard
arcaneInitiate cardId = (asset cardId "01063" "Arcane Initiate" 1 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = [Ally, Sorcerer]
  }

drawnToTheFlame :: CardId -> PlayerCard
drawnToTheFlame cardId = (event cardId "01064" "Drawn to the Flame" 0 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = [Insight]
  }

wardOfProtection :: CardId -> PlayerCard
wardOfProtection cardId = (event cardId "01065" "Ward of Protection" 1 Mystic)
  { pcSkills = [SkillWild]
  , pcTraits = [Spell, Spirit]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [WhenDrawTreachery You False]
  }

blindingLight :: CardId -> PlayerCard
blindingLight cardId = (event cardId "01066" "Blinding Light" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = [Spell]
  , pcAction = Just Action.Evade
  }

fearless :: CardId -> PlayerCard
fearless cardId = (skill cardId "01067" "Fearless" [SkillWillpower] Mystic)
  { pcTraits = [Innate]
  }

leatherCoat :: CardId -> PlayerCard
leatherCoat cardId = (asset cardId "01072" "Leather Coat" 0 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Armor]
  }

scavenging :: CardId -> PlayerCard
scavenging cardId = (asset cardId "01073" "Scavending" 1 Survivor)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Talent]
  }

baseballBat :: CardId -> PlayerCard
baseballBat cardId = (asset cardId "01074" "Baseball Bat" 2 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }

rabbitsFoot :: CardId -> PlayerCard
rabbitsFoot cardId = (asset cardId "01075" "Rabbit's Foot" 1 Survivor)
  { pcSkills = [SkillWild]
  , pcTraits = [Item, Charm]
  }

strayCat :: CardId -> PlayerCard
strayCat cardId = (asset cardId "01076" "Stray Cat" 1 Survivor)
  { pcSkills = [SkillAgility]
  , pcTraits = [Ally, Creature]
  }

digDeep :: CardId -> PlayerCard
digDeep cardId = (asset cardId "01077" "Dig Deep" 2 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = [Talent]
  }

cunningDistraction :: CardId -> PlayerCard
cunningDistraction cardId =
  (event cardId "01078" "Cunning Distraction" 5 Survivor)
    { pcTraits = [Tactic]
    , pcAction = Just Action.Evade
    }

lucky :: CardId -> PlayerCard
lucky cardId = (event cardId "01080" "Lucky!" 1 Survivor)
  { pcTraits = [Fortune]
  , pcFast = True
  , pcFastWindows = HashSet.fromList [WhenWouldFailSkillTest You]
  }

knife :: CardId -> PlayerCard
knife cardId = (asset cardId "01086" "Knife" 1 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = [Item, Weapon, Melee]
  }

flashlight :: CardId -> PlayerCard
flashlight cardId = (asset cardId "01087" "Flashlight" 2 Neutral)
  { pcSkills = [SkillIntellect]
  , pcTraits = [Item, Tool]
  }

emergencyCache :: CardId -> PlayerCard
emergencyCache cardId =
  (event cardId "01088" "Emergency Cache" 0 Neutral) { pcTraits = [Supply] }

guts :: CardId -> PlayerCard
guts cardId =
  (skill cardId "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { pcTraits = [Innate]
    }

perception :: CardId -> PlayerCard
perception cardId =
  (skill cardId "01090" "Perceptions" [SkillIntellect, SkillIntellect] Neutral)
    { pcTraits = [Practiced]
    }

overpower :: CardId -> PlayerCard
overpower cardId =
  (skill cardId "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
    { pcTraits = [Practiced]
    }

manualDexterity :: CardId -> PlayerCard
manualDexterity cardId =
  (skill cardId "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { pcTraits = [Innate]
    }

unexpectedCourage :: CardId -> PlayerCard
unexpectedCourage cardId =
  (skill cardId "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { pcTraits = [Innate]
    }

amnesia :: CardId -> PlayerCard
amnesia cardId = (treachery cardId "01096" "Amnesia" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

paranoia :: CardId -> PlayerCard
paranoia cardId = (treachery cardId "01097" "Paranoia" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

psychosis :: CardId -> PlayerCard
psychosis cardId = (treachery cardId "01099" "Psychosis" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

hypochondria :: CardId -> PlayerCard
hypochondria cardId = (treachery cardId "01100" "Hypochondria" 0)
  { pcTraits = [Madness]
  , pcRevelation = True
  }

silverTwilightAcolyte :: CardId -> PlayerCard
silverTwilightAcolyte cardId =
  (enemy cardId "01102" "Silver Twilight Acolyte" 0)
    { pcTraits = [Humanoid, Cultist, SilverTwilight]
    }

litaChantler :: CardId -> PlayerCard
litaChantler cardId =
  (asset cardId "01117" "Lita Chantler" 0 Neutral) { pcTraits = [Ally] }
