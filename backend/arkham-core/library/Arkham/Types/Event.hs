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
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Trait

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

data Event
  = OnTheLam' OnTheLam
  | DarkMemory' DarkMemory
  | Evidence' Evidence
  | Dodge' Dodge
  | DynamiteBlast' DynamiteBlast
  | ExtraAmmunition1' ExtraAmmunition1
  | MindOverMatter' MindOverMatter
  | WorkingAHunch' WorkingAHunch
  | Barricade' Barricade
  | CrypticResearch4' CrypticResearch4
  | Elusive' Elusive
  | Backstab' Backstab
  | SneakAttack' SneakAttack
  | SureGamble3' SureGamble3
  | HotStreak4' HotStreak4
  | DrawnToTheFlame' DrawnToTheFlame
  | WardOfProtection' WardOfProtection
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | CunningDistraction' CunningDistraction
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | CloseCall2' CloseCall2
  | Lucky2' Lucky2
  | WillToSurvive4' WillToSurvive4
  | EmergencyCache' EmergencyCache
  | SearchForTheTruth' SearchForTheTruth
  | Taunt' Taunt
  | Teamwork' Teamwork
  | Taunt2' Taunt2
  | Shortcut' Shortcut
  | SeekingAnswers' SeekingAnswers
  | ThinkOnYourFeet' ThinkOnYourFeet
  | BindMonster2' BindMonster2
  | BaitAndSwitch' BaitAndSwitch
  | EmergencyAid' EmergencyAid
  | IveGotAPlan' IveGotAPlan
  | Contraband' Contraband
  | DelveTooDeep' DelveTooDeep
  | Oops' Oops
  | LetMeHandleThis' LetMeHandleThis
  | SecondWind' SecondWind
  | BloodRite' BloodRite
  | AstoundingRevelation' AstoundingRevelation
  | FirstWatch' FirstWatch
  | DynamiteBlast2' DynamiteBlast2
  | Barricade3' Barricade3
  | HotStreak2' HotStreak2
  | MindWipe3' MindWipe3
  | Contraband2' Contraband2
  | Taunt3' Taunt3
  | IveGotAPlan2' IveGotAPlan2
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance
  ( HasCount ActionTakenCount env InvestigatorId
  , GetCardDef env EnemyId
  , HasSet Trait env EnemyId
  , HasSet AssetId env (InvestigatorId, UseType)
  , HasId LocationId env InvestigatorId
  , HasSet EnemyId env LocationId
  , HasSet EnemyId env InvestigatorId
  , HasSkillTest env
  )
  => HasActions env Event
deriving anyclass instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event
deriving anyclass instance EventRunner env => RunMessage env Event

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs

instance NamedEntity Event where
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
  [ OnTheLam' <$> onTheLam
  , DarkMemory' <$> darkMemory
  , Evidence' <$> evidence
  , Dodge' <$> dodge
  , DynamiteBlast' <$> dynamiteBlast
  , ExtraAmmunition1' <$> extraAmmunition1
  , MindOverMatter' <$> mindOverMatter
  , WorkingAHunch' <$> workingAHunch
  , Barricade' <$> barricade
  , CrypticResearch4' <$> crypticResearch4
  , Elusive' <$> elusive
  , Backstab' <$> backstab
  , SneakAttack' <$> sneakAttack
  , SureGamble3' <$> sureGamble3
  , HotStreak4' <$> hotStreak4
  , DrawnToTheFlame' <$> drawnToTheFlame
  , WardOfProtection' <$> wardOfProtection
  , BlindingLight' <$> blindingLight
  , MindWipe1' <$> mindWipe1
  , BlindingLight2' <$> blindingLight2
  , CunningDistraction' <$> cunningDistraction
  , LookWhatIFound' <$> lookWhatIFound
  , Lucky' <$> lucky
  , CloseCall2' <$> closeCall2
  , Lucky2' <$> lucky2
  , WillToSurvive4' <$> willToSurvive4
  , EmergencyCache' <$> emergencyCache
  , SearchForTheTruth' <$> searchForTheTruth
  , Taunt' <$> taunt
  , Teamwork' <$> teamwork
  , Taunt2' <$> taunt2
  , Shortcut' <$> shortcut
  , SeekingAnswers' <$> seekingAnswers
  , ThinkOnYourFeet' <$> thinkOnYourFeet
  , BindMonster2' <$> bindMonster2
  , BaitAndSwitch' <$> baitAndSwitch
  , EmergencyAid' <$> emergencyAid
  , IveGotAPlan' <$> iveGotAPlan
  , Contraband' <$> contraband
  , DelveTooDeep' <$> delveTooDeep
  , Oops' <$> oops
  , LetMeHandleThis' <$> letMeHandleThis
  , SecondWind' <$> secondWind
  , BloodRite' <$> bloodRite
  , AstoundingRevelation' <$> astoundingRevelation
  , FirstWatch' <$> firstWatch
  , DynamiteBlast2' <$> dynamiteBlast2
  , Barricade3' <$> barricade3
  , HotStreak2' <$> hotStreak2
  , MindWipe3' <$> mindWipe3
  , Contraband2' <$> contraband2
  , Taunt3' <$> taunt3
  , IveGotAPlan2' <$> iveGotAPlan2
  ]

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
