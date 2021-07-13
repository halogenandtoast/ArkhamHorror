module Arkham.Types.Event
  ( module Arkham.Types.Event
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Uses (UseType)
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Cards
import Arkham.Types.Event.Runner
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Trait

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

data Event
  = AChanceEncounter' AChanceEncounter
  | AceInTheHole3' AceInTheHole3
  | AstoundingRevelation' AstoundingRevelation
  | Backstab' Backstab
  | BaitAndSwitch' BaitAndSwitch
  | Barricade' Barricade
  | Barricade3' Barricade3
  | BindMonster2' BindMonster2
  | BlindingLight' BlindingLight
  | BlindingLight2' BlindingLight2
  | BloodRite' BloodRite
  | CloseCall2' CloseCall2
  | Contraband' Contraband
  | Contraband2' Contraband2
  | CrypticResearch4' CrypticResearch4
  | CunningDistraction' CunningDistraction
  | DarkMemory' DarkMemory
  | DecipheredReality5' DecipheredReality5
  | DelveTooDeep' DelveTooDeep
  | Dodge' Dodge
  | DrawnToTheFlame' DrawnToTheFlame
  | DynamiteBlast' DynamiteBlast
  | DynamiteBlast2' DynamiteBlast2
  | Elusive' Elusive
  | EmergencyAid' EmergencyAid
  | EmergencyCache' EmergencyCache
  | EmergencyCache2' EmergencyCache2
  | Evidence' Evidence
  | ExposeWeakness1' ExposeWeakness1
  | ExtraAmmunition1' ExtraAmmunition1
  | FirstWatch' FirstWatch
  | Flare1' Flare1
  | HotStreak2' HotStreak2
  | HotStreak4' HotStreak4
  | HypnoticGaze' HypnoticGaze
  | IfItBleeds' IfItBleeds
  | ImOuttaHere' ImOuttaHere
  | IveGotAPlan' IveGotAPlan
  | IveGotAPlan2' IveGotAPlan2
  | IveHadWorse4' IveHadWorse4
  | LetMeHandleThis' LetMeHandleThis
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | Lucky2' Lucky2
  | Lure1' Lure1
  | MindOverMatter' MindOverMatter
  | MindWipe1' MindWipe1
  | MindWipe3' MindWipe3
  | MomentOfRespite3' MomentOfRespite3
  | MonsterSlayer5' MonsterSlayer5
  | MoonlightRitual' MoonlightRitual
  | OnTheLam' OnTheLam
  | Oops' Oops
  | PreparedForTheWorst' PreparedForTheWorst
  | PreposterousSketches' PreposterousSketches
  | PreposterousSketches2' PreposterousSketches2
  | SearchForTheTruth' SearchForTheTruth
  | SecondWind' SecondWind
  | SeekingAnswers' SeekingAnswers
  | Shortcut' Shortcut
  | SneakAttack' SneakAttack
  | StandTogether3' StandTogether3
  | SureGamble3' SureGamble3
  | Taunt' Taunt
  | Taunt2' Taunt2
  | Taunt3' Taunt3
  | Teamwork' Teamwork
  | ThinkOnYourFeet' ThinkOnYourFeet
  | WardOfProtection' WardOfProtection
  | WardOfProtection5' WardOfProtection5
  | WillToSurvive3' WillToSurvive3
  | WorkingAHunch' WorkingAHunch
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance
  ( HasCount ActionTakenCount env InvestigatorId
  , GetCardDef env EnemyId
  , HasActions env ActionType
  , HasSet Trait env EnemyId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasId LocationId env InvestigatorId
  , HasSet EnemyId env LocationId
  , HasSet EnemyId env InvestigatorId
  , HasSkillTest env
  )
  => HasActions env Event

instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event where
  getModifiersFor = genericGetModifiersFor

deriving anyclass instance
  ( EventRunner env
  , HasSet FightableEnemyId env (InvestigatorId, Source)
  , HasCount HealthDamageCount env EnemyId
  , HasCount SanityDamageCount env EnemyId
  , HasCount DoomCount env AssetId
  , HasCount DoomCount env InvestigatorId
  , HasCount Shroud env LocationId
  , HasList DiscardedPlayerCard env InvestigatorId
  , HasCount FightCount env EnemyId
  )
  => RunMessage env Event

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs

instance Named Event where
  toName = toName . toAttrs

instance TargetEntity Event where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Event where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Event where
  toCardId = toCardId . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode) $ lookup cardCode allEvents

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  [ AChanceEncounter' <$> aChanceEncounter
  , AceInTheHole3' <$> aceInTheHole3
  , AstoundingRevelation' <$> astoundingRevelation
  , Backstab' <$> backstab
  , BaitAndSwitch' <$> baitAndSwitch
  , Barricade' <$> barricade
  , Barricade3' <$> barricade3
  , BindMonster2' <$> bindMonster2
  , BlindingLight' <$> blindingLight
  , BlindingLight2' <$> blindingLight2
  , BloodRite' <$> bloodRite
  , CloseCall2' <$> closeCall2
  , Contraband' <$> contraband
  , Contraband2' <$> contraband2
  , CrypticResearch4' <$> crypticResearch4
  , CunningDistraction' <$> cunningDistraction
  , DarkMemory' <$> darkMemory
  , DecipheredReality5' <$> decipheredReality5
  , DelveTooDeep' <$> delveTooDeep
  , Dodge' <$> dodge
  , DrawnToTheFlame' <$> drawnToTheFlame
  , DynamiteBlast' <$> dynamiteBlast
  , DynamiteBlast2' <$> dynamiteBlast2
  , Elusive' <$> elusive
  , EmergencyAid' <$> emergencyAid
  , EmergencyCache' <$> emergencyCache
  , EmergencyCache2' <$> emergencyCache2
  , Evidence' <$> evidence
  , ExposeWeakness1' <$> exposeWeakness1
  , ExtraAmmunition1' <$> extraAmmunition1
  , FirstWatch' <$> firstWatch
  , Flare1' <$> flare1
  , HotStreak2' <$> hotStreak2
  , HotStreak4' <$> hotStreak4
  , HypnoticGaze' <$> hypnoticGaze
  , IfItBleeds' <$> ifItBleeds
  , ImOuttaHere' <$> imOuttaHere
  , IveGotAPlan' <$> iveGotAPlan
  , IveGotAPlan2' <$> iveGotAPlan2
  , IveHadWorse4' <$> iveHadWorse4
  , LetMeHandleThis' <$> letMeHandleThis
  , LookWhatIFound' <$> lookWhatIFound
  , Lucky' <$> lucky
  , Lucky2' <$> lucky2
  , Lure1' <$> lure1
  , MindOverMatter' <$> mindOverMatter
  , MindWipe1' <$> mindWipe1
  , MindWipe3' <$> mindWipe3
  , MomentOfRespite3' <$> momentOfRespite3
  , MonsterSlayer5' <$> monsterSlayer5
  , MoonlightRitual' <$> moonlightRitual
  , OnTheLam' <$> onTheLam
  , Oops' <$> oops
  , PreparedForTheWorst' <$> preparedForTheWorst
  , PreposterousSketches' <$> preposterousSketches
  , PreposterousSketches2' <$> preposterousSketches2
  , SearchForTheTruth' <$> searchForTheTruth
  , SecondWind' <$> secondWind
  , SeekingAnswers' <$> seekingAnswers
  , Shortcut' <$> shortcut
  , SneakAttack' <$> sneakAttack
  , StandTogether3' <$> standTogether3
  , SureGamble3' <$> sureGamble3
  , Taunt' <$> taunt
  , Taunt2' <$> taunt2
  , Taunt3' <$> taunt3
  , Teamwork' <$> teamwork
  , ThinkOnYourFeet' <$> thinkOnYourFeet
  , WardOfProtection' <$> wardOfProtection
  , WardOfProtection5' <$> wardOfProtection5
  , WillToSurvive3' <$> willToSurvive3
  , WorkingAHunch' <$> workingAHunch
  ]

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
