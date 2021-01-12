module Arkham.Types.Card.PlayerCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Action (Action)
import qualified Arkham.Types.Action as Action
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.Class
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.ClassSymbol
import Arkham.Types.CommitRestriction
import Arkham.Types.Keyword (Keyword)
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.SkillType
import Arkham.Types.Trait
import Arkham.Types.Window

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  | PlayerEnemyType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype BearerId = BearerId { unBearerId :: CardCode }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, IsString)

newtype DiscardedPlayerCard = DiscardedPlayerCard { unDiscardedPlayerCard :: PlayerCard }

data AttackOfOpportunityModifier = DoesNotProvokeAttacksOfOpportunity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data PlayerCard = MkPlayerCard
  { pcCardCode :: CardCode
  , pcName :: Text
  , pcCost :: CardCost
  , pcLevel :: Int
  , pcCardType :: PlayerCardType
  , pcWeakness :: Bool
  , pcBearer :: Maybe BearerId
  , pcClassSymbol :: ClassSymbol
  , pcSkills :: [SkillType]
  , pcTraits :: HashSet Trait
  , pcKeywords :: HashSet Keyword
  , pcFast :: Bool
  , pcWindows :: HashSet Window
  , pcId :: CardId
  , pcAction :: Maybe Action
  , pcRevelation :: Bool
  , pcVictoryPoints :: Maybe Int
  , pcCommitRestrictions :: [CommitRestriction]
  , pcAttackOfOpportunityModifiers :: [AttackOfOpportunityModifier]
  , pcPermanent :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance ToJSON PlayerCard where
  toJSON = genericToJSON $ aesonOptions $ Just "pc"
  toEncoding = genericToEncoding $ aesonOptions $ Just "pc"

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "pc"

instance HasSkillIcons PlayerCard where
  getSkillIcons = pcSkills

instance HasCost PlayerCard where
  getCost c = case pcCost c of
    StaticCost n -> n
    DynamicCost -> 0

traits :: Lens' PlayerCard (HashSet Trait)
traits = lens pcTraits $ \m x -> m { pcTraits = x }

genPlayerCard :: MonadRandom m => CardCode -> m PlayerCard
genPlayerCard cardCode = lookupPlayerCard cardCode <$> getRandom

lookupPlayerCard :: CardCode -> (CardId -> PlayerCard)
lookupPlayerCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allPlayerCards

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
  , pcCost = StaticCost cost
  , pcLevel = 0
  , pcCardType = cardType
  , pcWeakness = False
  , pcBearer = Nothing
  , pcClassSymbol = classSymbol
  , pcSkills = mempty
  , pcTraits = mempty
  , pcKeywords = mempty
  , pcFast = False
  , pcWindows = mempty
  , pcId = cardId
  , pcAction = Nothing
  , pcRevelation = False
  , pcVictoryPoints = Nothing
  , pcCommitRestrictions = mempty
  , pcAttackOfOpportunityModifiers = mempty
  , pcPermanent = False
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
allPlayerCards = mapFromList
  [ ("asset", placeholderAsset)
  , ("01006", rolands38Special)
  , ("01007", coverUp)
  , ("01008", daisysToteBag)
  , ("01009", theNecronomicon)
  , ("01010", onTheLam)
  , ("01011", hospitalDebts)
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
  , ("01026", extraAmmunition1)
  , ("01027", policeBadge2)
  , ("01028", beatCop2)
  , ("01029", shotgun4)
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
  , ("01040", magnifyingGlass1)
  , ("01041", discOfItzamna2)
  , ("01042", encyclopedia2)
  , ("01043", crypticResearch4)
  , ("01044", switchblade)
  , ("01045", burglary)
  , ("01046", pickpoketing)
  , ("01047", fortyOneDerringer)
  , ("01048", leoDeLuca)
  , ("01049", hardKnocks)
  , ("01050", elusive)
  , ("01051", backstab)
  , ("01052", sneakAttack)
  , ("01053", opportunist)
  , ("01054", leoDeLuca1)
  , ("01055", catBurgler1)
  , ("01056", sureGamble3)
  , ("01057", hotStreak4)
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
  , ("01068", mindWipe1)
  , ("01069", blindingLight2)
  , ("01070", bookOfShadows3)
  , ("01071", grotesqueStatue4)
  , ("01072", leatherCoat)
  , ("01073", scavenging)
  , ("01074", baseballBat)
  , ("01075", rabbitsFoot)
  , ("01076", strayCat)
  , ("01077", digDeep)
  , ("01078", cunningDistraction)
  , ("01079", lookWhatIFound)
  , ("01080", lucky)
  , ("01081", survivalInstinct)
  , ("01082", aquinnah1)
  , ("01083", closeCall2)
  , ("01084", lucky2)
  , ("01085", willToSurvive4)
  , ("01086", knife)
  , ("01087", flashlight)
  , ("01088", emergencyCache)
  , ("01089", guts)
  , ("01090", perception)
  , ("01091", overpower)
  , ("01092", manualDexterity)
  , ("01093", unexpectedCourage)
  , ("01094", bulletproofVest3)
  , ("01095", elderSignAmulet3)
  , ("01096", amnesia)
  , ("01097", paranoia)
  , ("01098", haunted)
  , ("01099", psychosis)
  , ("01100", hypochondria)
  , ("01101", mobEnforcer)
  , ("01102", silverTwilightAcolyte)
  , ("01103", stubbornDetective)
  , ("01117", litaChantler)
  , ("02006", zoeysCross)
  , ("02007", smiteTheWicked)
  , ("02008", searchForTheTruth)
  , ("02009", rexsCurse)
  , ("02010", jennysTwin45s)
  , ("02011", searchingForIzzie)
  , ("02012", jimsTrumpet)
  , ("02013", finalRhapsody)
  , ("02014", duke)
  , ("02015", wrackedByNightmares)
  , ("02016", blackjack)
  , ("02017", taunt)
  , ("02018", teamwork)
  , ("02019", taunt2)
  , ("02020", laboratoryAssistant)
  , ("02021", strangeSolution)
  , ("02022", shortcut)
  , ("02023", seekingAnswers)
  , ("02024", liquidCourage)
  , ("02025", thinkOnYourFeet)
  , ("02026", doubleOrNothing)
  , ("02027", hiredMuscle1)
  , ("02028", riteOfSeeking)
  , ("02029", ritualCandles)
  , ("02030", clarityOfMind)
  , ("02031", bindMonster)
  , ("02032", fireAxe)
  , ("02033", peterSylvestre)
  , ("02034", baitAndSwitch)
  , ("02035", peterSylvestre2)
  , ("02036", kukri)
  , ("02037", indebted)
  , ("02038", internalInjury)
  , ("02039", chronophobia)
  , ("02040", drHenryArmitage)
  , ("02059", alchemicalConcoction)
  , ("02060", jazzMulligan)
  , ("02061", professorWarrenRice)
  , ("02079", peterClover)
  , ("02080", drFrancisMorgan)
  , ("02105", emergencyAid)
  , ("02106", brotherXavier1)
  , ("02107", iveGotAPlan)
  , ("02108", pathfinder1)
  , ("02109", contraband)
  , ("02110", adaptable1)
  , ("02111", delveTooDeep)
  , ("02139", adamLynch)
  , ("02140", theNecronomiconOlausWormiusTranslation)
  , ("02147", bandolier)
  , ("02185", keenEye3)
  , ("02185", springfieldM19034)
  , ("02301", lightningGun5)
  , ("03022", letMeHandleThis)
  , ("04023", toothOfEztli)
  , ("04149", secondWind)
  , ("04153", trueUnderstanding)
  , ("05316", occultLexicon)
  , ("05317", bloodRite)
  , ("06023", astoundingRevelation)
  , ("06110", firstWatch)
  , ("06116", scrollOfProphecies)
  , ("07152", keenEye)
  , ("50001", physicalTraining2)
  , ("50002", dynamiteBlast2)
  , ("50003", hyperawareness2)
  , ("50004", barricade3)
  , ("50005", hardKnocks2)
  , ("50006", hotStreak2)
  , ("50007", arcaneStudies2)
  , ("50008", mindWipe3)
  , ("50009", digDeep2)
  , ("50010", rabbitsFoot3)
  , ("51005", contraband2)
  , ("60130", taunt3)
  , ("60205", arcaneEnlightenment)
  , ("60206", celaenoFragments)
  , ("60208", encyclopedia)
  , ("60211", higherEducation)
  , ("60213", whittonGreene)
  , ("60225", iveGotAPlan2)
  , ("60504", atychiphobia)
  , ("81019", ladyEsprit)
  , ("81020", bearTrap)
  , ("81021", fishingNet)
  , ("81030", monstrousTransformation)
  , ("90002", daisysToteBagAdvanced)
  , ("90003", theNecronomiconAdvanced)
  ]

placeholderAsset :: CardId -> PlayerCard
placeholderAsset cardId = asset cardId "asset" "Placeholder Asset" 0 Neutral

rolands38Special :: CardId -> PlayerCard
rolands38Special cardId =
  (asset cardId "01006" "Roland's .38 Special" 3 Neutral)
    { pcSkills = [SkillCombat, SkillAgility, SkillWild]
    , pcTraits = setFromList [Item, Weapon, Firearm]
    }

coverUp :: CardId -> PlayerCard
coverUp cardId = (treachery cardId "01007" "Cover Up" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

daisysToteBag :: CardId -> PlayerCard
daisysToteBag cardId = (asset cardId "01008" "Daisy's Tote Bag" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Item]
  }

theNecronomicon :: CardId -> PlayerCard
theNecronomicon cardId = (asset cardId "01009" "The Necronomicon" 0 Neutral)
  { pcTraits = setFromList [Item, Tome]
  , pcWeakness = True
  , pcRevelation = True
  }

onTheLam :: CardId -> PlayerCard
onTheLam cardId = (event cardId "01010" "On the Lam" 1 Neutral)
  { pcTraits = setFromList [Tactic]
  , pcSkills = [SkillIntellect, SkillAgility, SkillWild, SkillWild]
  , pcFast = True
  , pcWindows = setFromList [AfterTurnBegins You, DuringTurn You]
  }

hospitalDebts :: CardId -> PlayerCard
hospitalDebts cardId = (treachery cardId "01011" "Hospital Debts" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

heirloomOfHyperborea :: CardId -> PlayerCard
heirloomOfHyperborea cardId =
  (asset cardId "01012" "Heirloom of Hyperborea" 3 Neutral)
    { pcSkills = [SkillWillpower, SkillCombat, SkillWild]
    , pcTraits = setFromList [Item, Relic]
    }

darkMemory :: CardId -> PlayerCard
darkMemory cardId = (event cardId "01013" "Dark Memory" 2 Neutral)
  { pcTraits = setFromList [Spell]
  , pcWeakness = True
  }

wendysAmulet :: CardId -> PlayerCard
wendysAmulet cardId = (asset cardId "01014" "Wendy's Amulet" 2 Neutral)
  { pcSkills = [SkillWild, SkillWild]
  , pcTraits = setFromList [Item, Relic]
  }

abandonedAndAlone :: CardId -> PlayerCard
abandonedAndAlone cardId = (treachery cardId "01015" "Abandoned and Alone" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

fortyFiveAutomatic :: CardId -> PlayerCard
fortyFiveAutomatic cardId = (asset cardId "01016" ".45 Automatic" 4 Guardian)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  }

physicalTraining :: CardId -> PlayerCard
physicalTraining cardId = (asset cardId "01017" "Physical Training" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Talent]
  }

beatCop :: CardId -> PlayerCard
beatCop cardId = (asset cardId "01018" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Police]
  }

firstAid :: CardId -> PlayerCard
firstAid cardId = (asset cardId "01019" "First Aid" 2 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Talent, Science]
  }

machete :: CardId -> PlayerCard
machete cardId = (asset cardId "01020" "Machete" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

guardDog :: CardId -> PlayerCard
guardDog cardId = (asset cardId "01021" "Guard Dog" 3 Guardian)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Creature]
  }

evidence :: CardId -> PlayerCard
evidence cardId = (event cardId "01022" "Evidence!" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyDefeated You]
  }

dodge :: CardId -> PlayerCard
dodge cardId = (event cardId "01023" "Dodge" 1 Guardian)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [WhenEnemyAttacks InvestigatorAtYourLocation]
  }

dynamiteBlast :: CardId -> PlayerCard
dynamiteBlast cardId = (event cardId "01024" "Dynamite Blast" 5 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Tactic]
  }

viciousBlow :: CardId -> PlayerCard
viciousBlow cardId =
  (skill cardId "01025" "Vicious Blow" [SkillCombat] Guardian)
    { pcTraits = setFromList [Practiced]
    }

extraAmmunition1 :: CardId -> PlayerCard
extraAmmunition1 cardId = (event cardId "01026" "Extra Ammunition" 2 Guardian)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Supply]
  , pcLevel = 1
  }

policeBadge2 :: CardId -> PlayerCard
policeBadge2 cardId = (asset cardId "01027" "Police Badge" 3 Guardian)
  { pcSkills = [SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item]
  , pcLevel = 2
  }

beatCop2 :: CardId -> PlayerCard
beatCop2 cardId = (asset cardId "01028" "Beat Cop" 4 Guardian)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Ally, Police]
  , pcLevel = 2
  }

shotgun4 :: CardId -> PlayerCard
shotgun4 cardId = (asset cardId "01029" "Shotgun" 5 Guardian)
  { pcSkills = [SkillCombat, SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 4
  }

magnifyingGlass :: CardId -> PlayerCard
magnifyingGlass cardId = (asset cardId "01030" "Magnifying Glass" 1 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

oldBookOfLore :: CardId -> PlayerCard
oldBookOfLore cardId = (asset cardId "01031" "Old Book of Lore" 3 Seeker)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Tome]
  }

researchLibrarian :: CardId -> PlayerCard
researchLibrarian cardId = (asset cardId "01032" "Research Librarian" 2 Seeker)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

drMilanChristopher :: CardId -> PlayerCard
drMilanChristopher cardId =
  (asset cardId "01033" "Dr. Milan Christopher" 4 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Ally, Miskatonic]
    }

hyperawareness :: CardId -> PlayerCard
hyperawareness cardId = (asset cardId "01034" "Hyperawareness" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

medicalTexts :: CardId -> PlayerCard
medicalTexts cardId = (asset cardId "01035" "Medical Texts" 2 Seeker)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Tome]
  }

mindOverMatter :: CardId -> PlayerCard
mindOverMatter cardId = (event cardId "01036" "Mind over Matter" 1 Seeker)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

workingAHunch :: CardId -> PlayerCard
workingAHunch cardId = (event cardId "01037" "Working a Hunch" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Insight]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

barricade :: CardId -> PlayerCard
barricade cardId = (event cardId "01038" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  }

deduction :: CardId -> PlayerCard
deduction cardId = (skill cardId "01039" "Deduction" [SkillIntellect] Seeker)
  { pcTraits = setFromList [Practiced]
  }

magnifyingGlass1 :: CardId -> PlayerCard
magnifyingGlass1 cardId = (asset cardId "01040" "Magnifying Glass" 0 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcLevel = 1
  }

discOfItzamna2 :: CardId -> PlayerCard
discOfItzamna2 cardId = (asset cardId "01041" "Disc of Itzamna" 3 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 2
  }

encyclopedia2 :: CardId -> PlayerCard
encyclopedia2 cardId = (asset cardId "01042" "Encyclopedia" 2 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Tome]
  , pcLevel = 2
  }

crypticResearch4 :: CardId -> PlayerCard
crypticResearch4 cardId = (event cardId "01043" "Cryptic Research" 0 Seeker)
  { pcTraits = setFromList [Insight]
  , pcLevel = 4
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

switchblade :: CardId -> PlayerCard
switchblade cardId = (asset cardId "01044" "Switchbalde" 1 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Item, Weapon, Melee, Illicit]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

burglary :: CardId -> PlayerCard
burglary cardId = (asset cardId "01045" "Burglary" 1 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Talent, Illicit]
  }

pickpoketing :: CardId -> PlayerCard
pickpoketing cardId = (asset cardId "01046" "Pickpocketing" 2 Rogue)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Talent, Illicit]
  }

fortyOneDerringer :: CardId -> PlayerCard
fortyOneDerringer cardId = (asset cardId "01047" ".41 Derringer" 3 Rogue)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Firearm, Illicit]
  }

leoDeLuca :: CardId -> PlayerCard
leoDeLuca cardId = (asset cardId "01048" "Leo De Luca" 6 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Criminal]
  }

hardKnocks :: CardId -> PlayerCard
hardKnocks cardId = (asset cardId "01049" "Hard Knocks" 2 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

elusive :: CardId -> PlayerCard
elusive cardId = (event cardId "01050" "Elusive" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

backstab :: CardId -> PlayerCard
backstab cardId = (event cardId "01051" "Backstab" 3 Rogue)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcAction = Just Action.Fight
  }

sneakAttack :: CardId -> PlayerCard
sneakAttack cardId = (event cardId "01052" "Sneak Attack" 2 Rogue)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Tactic]
  }

opportunist :: CardId -> PlayerCard
opportunist cardId = (skill cardId "01053" "Opportunist" [SkillWild] Rogue)
  { pcTraits = setFromList [Innate]
  , pcCommitRestrictions = [OnlyYourTest]
  }

leoDeLuca1 :: CardId -> PlayerCard
leoDeLuca1 cardId = (asset cardId "01054" "Leo De Luca" 5 Rogue)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

catBurgler1 :: CardId -> PlayerCard
catBurgler1 cardId = (asset cardId "01055" "Cat Burgler" 4 Rogue)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

sureGamble3 :: CardId -> PlayerCard
sureGamble3 cardId = (asset cardId "01056" "Sure Gamble" 2 Rogue)
  { pcTraits = setFromList [Fortune, Insight]
  , pcFast = True
  , pcWindows = mempty -- We handle this via behavior
  , pcLevel = 3
  }

hotStreak4 :: CardId -> PlayerCard
hotStreak4 cardId = (event cardId "01057" "Hot Streak" 2 Rogue)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Fortune]
  , pcLevel = 4
  }

forbiddenKnowledge :: CardId -> PlayerCard
forbiddenKnowledge cardId =
  (asset cardId "01058" "Forbidden Knowledge" 0 Mystic)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Talent]
    }

holyRosary :: CardId -> PlayerCard
holyRosary cardId = (asset cardId "01059" "Holy Rosary" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Charm]
  }

shrivelling :: CardId -> PlayerCard
shrivelling cardId = (asset cardId "01060" "Shrivelling" 3 Mystic)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Spell]
  }

scrying :: CardId -> PlayerCard
scrying cardId = (asset cardId "01061" "Scrying" 1 Mystic)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spell]
  }

arcaneStudies :: CardId -> PlayerCard
arcaneStudies cardId = (asset cardId "01062" "Arcane Studies" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

arcaneInitiate :: CardId -> PlayerCard
arcaneInitiate cardId = (asset cardId "01063" "Arcane Initiate" 1 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Sorcerer]
  }

drawnToTheFlame :: CardId -> PlayerCard
drawnToTheFlame cardId = (event cardId "01064" "Drawn to the Flame" 0 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Insight]
  }

wardOfProtection :: CardId -> PlayerCard
wardOfProtection cardId = (event cardId "01065" "Ward of Protection" 1 Mystic)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Spell, Spirit]
  , pcFast = True
  , pcWindows = setFromList [WhenDrawTreachery You]
  }

blindingLight :: CardId -> PlayerCard
blindingLight cardId = (event cardId "01066" "Blinding Light" 2 Mystic)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Spell]
  , pcAction = Just Action.Evade
  }

fearless :: CardId -> PlayerCard
fearless cardId = (skill cardId "01067" "Fearless" [SkillWillpower] Mystic)
  { pcTraits = setFromList [Innate]
  }

mindWipe1 :: CardId -> PlayerCard
mindWipe1 cardId = (event cardId "01068" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Spell]
  , pcLevel = 1
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }

blindingLight2 :: CardId -> PlayerCard
blindingLight2 cardId = (event cardId "01069" "Blinding Light" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Spell]
  , pcAction = Just Action.Evade
  , pcLevel = 2
  }

bookOfShadows3 :: CardId -> PlayerCard
bookOfShadows3 cardId = (asset cardId "01070" "Book of Shadows" 4 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Item, Tome]
  , pcLevel = 3
  }

grotesqueStatue4 :: CardId -> PlayerCard
grotesqueStatue4 cardId = (asset cardId "01071" "Grotesque Statue" 2 Mystic)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 4
  }

leatherCoat :: CardId -> PlayerCard
leatherCoat cardId = (asset cardId "01072" "Leather Coat" 0 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Armor]
  }

scavenging :: CardId -> PlayerCard
scavenging cardId = (asset cardId "01073" "Scavending" 1 Survivor)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

baseballBat :: CardId -> PlayerCard
baseballBat cardId = (asset cardId "01074" "Baseball Bat" 2 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

rabbitsFoot :: CardId -> PlayerCard
rabbitsFoot cardId = (asset cardId "01075" "Rabbit's Foot" 1 Survivor)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Charm]
  }

strayCat :: CardId -> PlayerCard
strayCat cardId = (asset cardId "01076" "Stray Cat" 1 Survivor)
  { pcSkills = [SkillAgility]
  , pcTraits = setFromList [Ally, Creature]
  }

digDeep :: CardId -> PlayerCard
digDeep cardId = (asset cardId "01077" "Dig Deep" 2 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Talent]
  }

cunningDistraction :: CardId -> PlayerCard
cunningDistraction cardId =
  (event cardId "01078" "Cunning Distraction" 5 Survivor)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = setFromList [Tactic]
    , pcAction = Just Action.Evade
    }

lookWhatIFound :: CardId -> PlayerCard
lookWhatIFound cardId =
  (event cardId "01079" "\"Look what I found!\"" 2 Survivor)
    { pcSkills = [SkillIntellect, SkillIntellect]
    , pcTraits = setFromList [Fortune]
    , pcFast = True
    , pcWindows = setFromList
      [ AfterFailInvestigationSkillTest You n | n <- [0 .. 2] ]
    }

lucky :: CardId -> PlayerCard
lucky cardId = (event cardId "01080" "Lucky!" 1 Survivor)
  { pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  }

survivalInstinct :: CardId -> PlayerCard
survivalInstinct cardId =
  (skill cardId "01081" "Survival Instrinct" [SkillAgility] Survivor)
    { pcTraits = setFromList [Innate]
    }

aquinnah1 :: CardId -> PlayerCard
aquinnah1 cardId = (asset cardId "01082" "Aquinnah" 5 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally]
  , pcLevel = 1
  }

closeCall2 :: CardId -> PlayerCard
closeCall2 cardId = (event cardId "01083" "Close Call" 2 Survivor)
  { pcSkills = [SkillCombat, SkillAgility]
  , pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = mempty -- We handle this via behavior
  , pcLevel = 2
  }

lucky2 :: CardId -> PlayerCard
lucky2 cardId = (event cardId "01084" "Lucky!" 1 Survivor)
  { pcTraits = setFromList [Fortune]
  , pcFast = True
  , pcWindows = setFromList [WhenWouldFailSkillTest You]
  , pcLevel = 2
  }

willToSurvive4 :: CardId -> PlayerCard
willToSurvive4 cardId = (event cardId "01085" "Will to Survive" 4 Survivor)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Spirit]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcLevel = 4
  }

knife :: CardId -> PlayerCard
knife cardId = (asset cardId "01086" "Knife" 1 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

flashlight :: CardId -> PlayerCard
flashlight cardId = (asset cardId "01087" "Flashlight" 2 Neutral)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tool]
  }

emergencyCache :: CardId -> PlayerCard
emergencyCache cardId = (event cardId "01088" "Emergency Cache" 0 Neutral)
  { pcTraits = setFromList [Supply]
  }

guts :: CardId -> PlayerCard
guts cardId =
  (skill cardId "01089" "Guts" [SkillWillpower, SkillWillpower] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

perception :: CardId -> PlayerCard
perception cardId =
  (skill cardId "01090" "Perceptions" [SkillIntellect, SkillIntellect] Neutral)
    { pcTraits = setFromList [Practiced]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

overpower :: CardId -> PlayerCard
overpower cardId =
  (skill cardId "01091" "Overpower" [SkillCombat, SkillCombat] Neutral)
    { pcTraits = setFromList [Practiced]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

manualDexterity :: CardId -> PlayerCard
manualDexterity cardId =
  (skill cardId "01092" "Manual Dexterity" [SkillAgility, SkillAgility] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

unexpectedCourage :: CardId -> PlayerCard
unexpectedCourage cardId =
  (skill cardId "01093" "Unexpected Courage" [SkillWild, SkillWild] Neutral)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [MaxOnePerTest]
    }

bulletproofVest3 :: CardId -> PlayerCard
bulletproofVest3 cardId = (asset cardId "01094" "Bulletproof Vest" 2 Neutral)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Item, Armor]
  , pcLevel = 3
  }

elderSignAmulet3 :: CardId -> PlayerCard
elderSignAmulet3 cardId = (asset cardId "01095" "Elder Sign Amulet" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item, Relic]
  , pcLevel = 3
  }

amnesia :: CardId -> PlayerCard
amnesia cardId = (treachery cardId "01096" "Amnesia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

paranoia :: CardId -> PlayerCard
paranoia cardId = (treachery cardId "01097" "Paranoia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

haunted :: CardId -> PlayerCard
haunted cardId = (treachery cardId "01098" "Haunted" 0)
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

psychosis :: CardId -> PlayerCard
psychosis cardId = (treachery cardId "01099" "Psychosis" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

hypochondria :: CardId -> PlayerCard
hypochondria cardId = (treachery cardId "01100" "Hypochondria" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

mobEnforcer :: CardId -> PlayerCard
mobEnforcer cardId = (enemy cardId "01101" "Mob Enforcer" 0)
  { pcTraits = setFromList [Humanoid, Criminal]
  , pcKeywords = setFromList [Keyword.Hunter]
  }

silverTwilightAcolyte :: CardId -> PlayerCard
silverTwilightAcolyte cardId =
  (enemy cardId "01102" "Silver Twilight Acolyte" 0)
    { pcTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , pcKeywords = setFromList [Keyword.Hunter]
    }

stubbornDetective :: CardId -> PlayerCard
stubbornDetective cardId = (enemy cardId "01103" "Stubborn Detective" 0)
  { pcTraits = setFromList [Humanoid, Detective]
  , pcKeywords = setFromList [Keyword.Hunter]
  }

zoeysCross :: CardId -> PlayerCard
zoeysCross cardId = (asset cardId "02006" "Zoey's Cross" 1 Neutral)
  { pcSkills = [SkillCombat, SkillCombat, SkillWild]
  , pcTraits = setFromList [Item, Charm]
  }

smiteTheWicked :: CardId -> PlayerCard
smiteTheWicked cardId = (treachery cardId "02007" "Smite the Wicked" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

searchForTheTruth :: CardId -> PlayerCard
searchForTheTruth cardId =
  (event cardId "02008" "Search for the Truth" 1 Neutral)
    { pcSkills = [SkillIntellect, SkillIntellect, SkillWild]
    , pcTraits = setFromList [Insight]
    }

rexsCurse :: CardId -> PlayerCard
rexsCurse cardId = (treachery cardId "02009" "Rex's Curse" 0)
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

jennysTwin45s :: CardId -> PlayerCard
jennysTwin45s cardId = (asset cardId "02010" "Jenny's Twin .45s" 0 Neutral)
  { pcSkills = [SkillAgility, SkillAgility, SkillWild]
  , pcTraits = setFromList [Item, Weapon, Firearm]
  , pcCost = DynamicCost
  }

searchingForIzzie :: CardId -> PlayerCard
searchingForIzzie cardId = (treachery cardId "02011" "Searching for Izzie" 0)
  { pcTraits = setFromList [Task]
  , pcRevelation = True
  }

jimsTrumpet :: CardId -> PlayerCard
jimsTrumpet cardId = (asset cardId "02012" "Jim's Trumpet" 2 Neutral)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillWild]
  , pcTraits = setFromList [Item, Instrument, Relic]
  }

finalRhapsody :: CardId -> PlayerCard
finalRhapsody cardId = (treachery cardId "02013" "Final Rhapsody" 0)
  { pcTraits = setFromList [Endtimes]
  , pcRevelation = True
  }

duke :: CardId -> PlayerCard
duke cardId = (asset cardId "02014" "Duke" 2 Neutral)
  { pcTraits = setFromList [Ally, Creature]
  }

wrackedByNightmares :: CardId -> PlayerCard
wrackedByNightmares cardId =
  (treachery cardId "02015" "Wracked by Nightmares" 0)
    { pcTraits = setFromList [Madness]
    , pcRevelation = True
    }

blackjack :: CardId -> PlayerCard
blackjack cardId = (asset cardId "02016" "Blackjack" 1 Guardian)
  { pcTraits = setFromList [Item, Weapon, Melee]
  , pcSkills = [SkillCombat]
  }

taunt :: CardId -> PlayerCard
taunt cardId = (event cardId "02017" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcSkills = [SkillWillpower, SkillCombat]
  }

teamwork :: CardId -> PlayerCard
teamwork cardId = (event cardId "02018" "Teamwork" 0 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcSkills = [SkillWild]
  }

taunt2 :: CardId -> PlayerCard
taunt2 cardId = (event cardId "02019" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  , pcSkills = [SkillWillpower, SkillCombat, SkillAgility]
  }

laboratoryAssistant :: CardId -> PlayerCard
laboratoryAssistant cardId =
  (asset cardId "02020" "Laboratory Assistant" 2 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Ally, Miskatonic, Science]
    }

strangeSolution :: CardId -> PlayerCard
strangeSolution cardId = (asset cardId "02021" "Strange Solution" 1 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Science]
  }

shortcut :: CardId -> PlayerCard
shortcut cardId = (event cardId "02022" "Shortcut" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  , pcFast = True
  , pcWindows = setFromList [DuringTurn You]
  }

seekingAnswers :: CardId -> PlayerCard
seekingAnswers cardId = (event cardId "02023" "Seeking Answers" 1 Seeker)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = singleton Insight
  }

liquidCourage :: CardId -> PlayerCard
liquidCourage cardId = (asset cardId "02024" "Liquid Courage" 1 Rogue)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Illicit]
  }

thinkOnYourFeet :: CardId -> PlayerCard
thinkOnYourFeet cardId = (event cardId "02025" "Think on Your Feet" 1 Rogue)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = singleton Trick
  , pcFast = True
  , pcWindows = setFromList [WhenEnemySpawns YourLocation []]
  }

doubleOrNothing :: CardId -> PlayerCard
doubleOrNothing cardId =
  (skill cardId "02026" "Double or Nothing" [SkillWild] Rogue)
    { pcTraits = singleton Fortune
    , pcCommitRestrictions = [MaxOnePerTest]
    }

hiredMuscle1 :: CardId -> PlayerCard
hiredMuscle1 cardId = (asset cardId "02027" "Hired Muscle" 1 Rogue)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Ally, Criminal]
  , pcLevel = 1
  }

riteOfSeeking :: CardId -> PlayerCard
riteOfSeeking cardId = (asset cardId "02028" "Rite of Seeking" 4 Mystic)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spell]
  }

ritualCandles :: CardId -> PlayerCard
ritualCandles cardId = (asset cardId "02029" "Ritual Candles" 1 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = singleton Item
  }

clarityOfMind :: CardId -> PlayerCard
clarityOfMind cardId = (asset cardId "02030" "Clarity of Mind" 2 Mystic)
  { pcSkills = [SkillWillpower]
  , pcTraits = singleton Spell
  }

bindMonster :: CardId -> PlayerCard
bindMonster cardId = (event cardId "02031" "Bind Monster" 3 Mystic)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = singleton Spell
  , pcAction = Just Action.Evade
  , pcLevel = 2
  }

fireAxe :: CardId -> PlayerCard
fireAxe cardId = (asset cardId "02032" "Fire Axe" 1 Survivor)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

peterSylvestre :: CardId -> PlayerCard
peterSylvestre cardId = (asset cardId "02033" "Peter Sylvestre" 3 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

baitAndSwitch :: CardId -> PlayerCard
baitAndSwitch cardId = (event cardId "02034" "Bait and Switch" 1 Survivor)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Trick]
  , pcAction = Just Action.Evade
  }

peterSylvestre2 :: CardId -> PlayerCard
peterSylvestre2 cardId = (asset cardId "02035" "Peter Sylvestre" 3 Survivor)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally, Miskatonic]
  , pcLevel = 2
  }

kukri :: CardId -> PlayerCard
kukri cardId = (asset cardId "02036" "Kukri" 2 Neutral)
  { pcSkills = [SkillCombat]
  , pcTraits = setFromList [Item, Weapon, Melee]
  }

indebted :: CardId -> PlayerCard
indebted cardId = (treachery cardId "02037" "Indebted" 0)
  { pcTraits = singleton Flaw
  , pcRevelation = True
  , pcPermanent = True
  }

internalInjury :: CardId -> PlayerCard
internalInjury cardId = (treachery cardId "02038" "Internal Injury" 0)
  { pcTraits = singleton Injury
  , pcRevelation = True
  }

chronophobia :: CardId -> PlayerCard
chronophobia cardId = (treachery cardId "02039" "Chronophobia" 0)
  { pcTraits = singleton Madness
  , pcRevelation = True
  }

drHenryArmitage :: CardId -> PlayerCard
drHenryArmitage cardId = (asset cardId "02040" "Dr. Henry Armitage" 2 Neutral)
  { pcSkills = [SkillWild, SkillWild]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

alchemicalConcoction :: CardId -> PlayerCard
alchemicalConcoction cardId =
  (asset cardId "02059" "Alchemical Concoction" 0 Neutral)
    { pcTraits = setFromList [Item, Science]
    }

jazzMulligan :: CardId -> PlayerCard
jazzMulligan cardId = (asset cardId "02060" "\"Jazz\" Mulligan" 0 Neutral)
  { pcTraits = setFromList [Ally, Miskatonic]
  }

professorWarrenRice :: CardId -> PlayerCard
professorWarrenRice cardId =
  (asset cardId "02061" "Progressor Warren Rice" 3 Neutral)
    { pcSkills = [SkillIntellect, SkillWild]
    , pcTraits = setFromList [Ally, Miskatonic]
    }

peterClover :: CardId -> PlayerCard
peterClover cardId = (asset cardId "02079" "Peter Clover" 0 Neutral)
  { pcTraits = setFromList [Humanoid, Criminal]
  }

drFrancisMorgan :: CardId -> PlayerCard
drFrancisMorgan cardId = (asset cardId "02080" "Dr. Francis Morgan" 3 Neutral)
  { pcSkills = [SkillCombat, SkillWild]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

emergencyAid :: CardId -> PlayerCard
emergencyAid cardId = (event cardId "02105" "Emergency Aid" 2 Guardian)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Science]
  }

brotherXavier1 :: CardId -> PlayerCard
brotherXavier1 cardId = (asset cardId "02106" "Brother Xavier" 5 Guardian)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Ally]
  , pcLevel = 1
  }

iveGotAPlan :: CardId -> PlayerCard
iveGotAPlan cardId = (event cardId "02107" "\"I've Got a Plan!\"" 3 Seeker)
  { pcSkills = [SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Insight, Tactic]
  }

pathfinder1 :: CardId -> PlayerCard
pathfinder1 cardId = (asset cardId "02108" "Pathfinder" 3 Seeker)
  { pcSkills = [SkillAgility]
  , pcTraits = singleton Talent
  , pcLevel = 1
  }

contraband :: CardId -> PlayerCard
contraband cardId = (event cardId "02109" "Contraband" 4 Rogue)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Supply, Illicit]
  }

adaptable1 :: CardId -> PlayerCard
adaptable1 cardId = (asset cardId "02110" "Adaptable" 0 Rogue)
  { pcPermanent = True
  , pcTraits = setFromList [Talent]
  }

delveTooDeep :: CardId -> PlayerCard
delveTooDeep cardId = (event cardId "02111" "Delve Too Deep" 1 Mystic)
  { pcTraits = setFromList [Insight]
  , pcVictoryPoints = Just 1
  }

adamLynch :: CardId -> PlayerCard
adamLynch cardId = (asset cardId "02139" "Adam Lynch" 0 Neutral)
  { pcTraits = setFromList [Ally, Miskatonic]
  }

keenEye :: CardId -> PlayerCard
keenEye cardId = (asset cardId "02185" "Keen Eye" 2 Guardian)
  { pcTraits = setFromList [Talent]
  , pcSkills = [SkillIntellect, SkillCombat]
  }

theNecronomiconOlausWormiusTranslation :: CardId -> PlayerCard
theNecronomiconOlausWormiusTranslation cardId =
  (asset cardId "02140" "The Necronomicon" 2 Neutral)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Item, Tome]
    }

bandolier :: CardId -> PlayerCard
bandolier cardId = (asset cardId "02147" "Bandolier" 2 Guardian)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Item]
  }

keenEye3 :: CardId -> PlayerCard
keenEye3 cardId = (asset cardId "02185" "Keen Eye" 0 Guardian)
  { pcTraits = setFromList [Talent]
  , pcPermanent = True
  , pcLevel = 3
  }

springfieldM19034 :: CardId -> PlayerCard
springfieldM19034 cardId = (asset cardId "02226" "Springfiled M1903" 4 Guardian
                           )
  { pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 4
  , pcSkills = [SkillCombat, SkillAgility]
  }

lightningGun5 :: CardId -> PlayerCard
lightningGun5 cardId = (asset cardId "02301" "Lightning Gun" 6 Guardian)
  { pcTraits = setFromList [Item, Weapon, Firearm]
  , pcLevel = 5
  , pcSkills = [SkillIntellect, SkillCombat]
  }

letMeHandleThis :: CardId -> PlayerCard
letMeHandleThis cardId =
  (event cardId "03022" "\"Let me handle this!\"" 0 Guardian)
    { pcSkills = [SkillWillpower, SkillCombat]
    , pcTraits = setFromList [Spirit]
    , pcFast = True
    , pcWindows = mempty -- We handle this via behavior
    }

toothOfEztli :: CardId -> PlayerCard
toothOfEztli cardId = (asset cardId "04023" "Tooth of Eztli" 3 Seeker)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Item, Relic]
  }

secondWind :: CardId -> PlayerCard
secondWind cardId = (event cardId "04149" "Second Wind" 1 Guardian)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Spirit, Bold]
  , pcFast = True -- not fast
  , pcWindows = mempty -- handle via behavior since must be first action
  }

trueUnderstanding :: CardId -> PlayerCard
trueUnderstanding cardId =
  (skill cardId "04153" "True Understanding" [SkillWild] Seeker)
    { pcTraits = setFromList [Innate]
    , pcCommitRestrictions = [ScenarioAbility]
    }

occultLexicon :: CardId -> PlayerCard
occultLexicon cardId = (asset cardId "05316" "Occult Lexicon" 2 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tome, Occult]
  }

bloodRite :: CardId -> PlayerCard
bloodRite cardId = (event cardId "05317" "Blood Rite" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Spell]
  }

firstWatch :: CardId -> PlayerCard
firstWatch cardId = (event cardId "06110" "First Watch" 1 Guardian)
  { pcSkills = [SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [WhenAllDrawEncounterCard]
  }

scrollOfProphecies :: CardId -> PlayerCard
scrollOfProphecies cardId =
  (asset cardId "06116" "Scroll of Prophecies" 3 Mystic)
    { pcSkills = [SkillWillpower]
    , pcTraits = setFromList [Item, Tome]
    }

astoundingRevelation :: CardId -> PlayerCard
astoundingRevelation cardId =
  (event cardId "06023" "Astounding Revelation" 0 Seeker)
    { pcSkills = [SkillIntellect]
    , pcTraits = setFromList [Research]
    , pcFast = True
    , pcWindows = mempty -- cannot be played
    }

litaChantler :: CardId -> PlayerCard
litaChantler cardId = (asset cardId "01117" "Lita Chantler" 0 Neutral)
  { pcTraits = setFromList [Ally]
  }

physicalTraining2 :: CardId -> PlayerCard
physicalTraining2 cardId = (asset cardId "50001" "Physical Training" 0 Guardian
                           )
  { pcSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillCombat]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

dynamiteBlast2 :: CardId -> PlayerCard
dynamiteBlast2 cardId = (event cardId "50002" "Dynamite Blast" 4 Guardian)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Tactic]
  , pcAttackOfOpportunityModifiers = [DoesNotProvokeAttacksOfOpportunity]
  , pcLevel = 2
  }

hyperawareness2 :: CardId -> PlayerCard
hyperawareness2 cardId = (asset cardId "50003" "Hyperawareness" 0 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

barricade3 :: CardId -> PlayerCard
barricade3 cardId = (event cardId "50004" "Barricade" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillAgility]
  , pcTraits = setFromList [Insight, Tactic]
  , pcLevel = 3
  }

hardKnocks2 :: CardId -> PlayerCard
hardKnocks2 cardId = (asset cardId "50005" "Hard Knocks" 0 Rogue)
  { pcSkills = [SkillCombat, SkillCombat, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

hotStreak2 :: CardId -> PlayerCard
hotStreak2 cardId = (event cardId "50006" "Hot Streak" 5 Rogue)
  { pcSkills = [SkillWillpower]
  , pcTraits = setFromList [Fortune]
  , pcLevel = 2
  }

arcaneStudies2 :: CardId -> PlayerCard
arcaneStudies2 cardId = (asset cardId "50007" "Arcane Studies" 0 Mystic)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

mindWipe3 :: CardId -> PlayerCard
mindWipe3 cardId = (event cardId "50008" "Mind Wipe" 1 Mystic)
  { pcSkills = [SkillWillpower, SkillCombat]
  , pcTraits = setFromList [Spell]
  , pcLevel = 3
  , pcFast = True
  , pcWindows = setFromList [AnyPhaseBegins]
  }

digDeep2 :: CardId -> PlayerCard
digDeep2 cardId = (asset cardId "50009" "Dig Deep" 0 Survivor)
  { pcSkills = [SkillWillpower, SkillWillpower, SkillAgility, SkillAgility]
  , pcTraits = setFromList [Talent]
  , pcLevel = 2
  }

rabbitsFoot3 :: CardId -> PlayerCard
rabbitsFoot3 cardId = (asset cardId "50010" "Rabbit's Foot" 1 Survivor)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Charm]
  , pcLevel = 3
  }

contraband2 :: CardId -> PlayerCard
contraband2 cardId = (event cardId "51005" "Contraband" 3 Rogue)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillIntellect]
  , pcTraits = setFromList [Supply, Illicit]
  , pcLevel = 2
  }

taunt3 :: CardId -> PlayerCard
taunt3 cardId = (event cardId "60130" "Taunt" 1 Guardian)
  { pcTraits = setFromList [Tactic]
  , pcFast = True
  , pcWindows = setFromList [FastPlayerWindow]
  , pcSkills = [SkillWillpower, SkillWillpower, SkillCombat, SkillAgility]
  }

arcaneEnlightenment :: CardId -> PlayerCard
arcaneEnlightenment cardId =
  (asset cardId "60205" "Arcane Enlightenment" 2 Seeker)
    { pcSkills = [SkillWillpower, SkillWillpower]
    , pcTraits = setFromList [Ritual]
    }

celaenoFragments :: CardId -> PlayerCard
celaenoFragments cardId = (asset cardId "60206" "Celaeno Fragments" 1 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Item, Tome]
  }

encyclopedia :: CardId -> PlayerCard
encyclopedia cardId = (asset cardId "60208" "Encyclopedia" 2 Seeker)
  { pcSkills = [SkillWild]
  , pcTraits = setFromList [Item, Tome]
  }

higherEducation :: CardId -> PlayerCard
higherEducation cardId = (asset cardId "60211" "Higher Education" 0 Seeker)
  { pcSkills = [SkillWillpower, SkillIntellect]
  , pcTraits = setFromList [Talent]
  }

whittonGreene :: CardId -> PlayerCard
whittonGreene cardId = (asset cardId "60213" "Whitton Greene" 4 Seeker)
  { pcSkills = [SkillIntellect]
  , pcTraits = setFromList [Ally, Miskatonic]
  }

iveGotAPlan2 :: CardId -> PlayerCard
iveGotAPlan2 cardId = (event cardId "60225" "\"I've Got a Plan!\"" 2 Seeker)
  { pcSkills = [SkillIntellect, SkillIntellect, SkillCombat]
  , pcTraits = setFromList [Insight, Tactic]
  }

atychiphobia :: CardId -> PlayerCard
atychiphobia cardId = (treachery cardId "60504" "Atychiphobia" 0)
  { pcTraits = setFromList [Madness]
  , pcRevelation = True
  }

ladyEsprit :: CardId -> PlayerCard
ladyEsprit cardId = (asset cardId "81019" "Lady Espirt" 4 Neutral)
  { pcSkills = [SkillWillpower, SkillIntellect, SkillWild]
  , pcTraits = setFromList [Ally, Sorcerer]
  }

bearTrap :: CardId -> PlayerCard
bearTrap cardId =
  (asset cardId "81020" "Bear Trap" 0 Neutral) { pcTraits = setFromList [Trap] }

fishingNet :: CardId -> PlayerCard
fishingNet cardId = (asset cardId "81021" "Fishing Net" 0 Neutral)
  { pcTraits = setFromList [Trap]
  }

curseOfTheRougarou :: CardId -> PlayerCard
curseOfTheRougarou cardId = (treachery cardId "81029" "Curse of the Rougarou" 0
                            )
  { pcTraits = setFromList [Curse]
  , pcRevelation = True
  }

monstrousTransformation :: CardId -> PlayerCard
monstrousTransformation cardId =
  (asset cardId "81030" "Monstrous Transformation" 0 Neutral)
    { pcTraits = setFromList [Talent]
    , pcFast = True
    , pcWindows = setFromList [DuringTurn You]
    }

daisysToteBagAdvanced :: CardId -> PlayerCard
daisysToteBagAdvanced cardId =
  (asset cardId "90002" "Daisy's Tote Bag" 2 Neutral)
    { pcSkills = [SkillWillpower, SkillIntellect, SkillWild, SkillWild]
    , pcTraits = setFromList [Item]
    }

theNecronomiconAdvanced :: CardId -> PlayerCard
theNecronomiconAdvanced cardId =
  (asset cardId "90003" "The Necronomicon" 0 Neutral)
    { pcTraits = setFromList [Item, Tome]
    , pcWeakness = True
    , pcRevelation = True
    }
