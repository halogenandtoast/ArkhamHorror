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
  = AstoundingRevelation' AstoundingRevelation
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
  | DelveTooDeep' DelveTooDeep
  | Dodge' Dodge
  | DrawnToTheFlame' DrawnToTheFlame
  | DynamiteBlast' DynamiteBlast
  | DynamiteBlast2' DynamiteBlast2
  | Elusive' Elusive
  | EmergencyAid' EmergencyAid
  | EmergencyCache' EmergencyCache
  | Evidence' Evidence
  | ExtraAmmunition1' ExtraAmmunition1
  | FirstWatch' FirstWatch
  | Flare1' Flare1
  | HotStreak2' HotStreak2
  | HotStreak4' HotStreak4
  | IveGotAPlan' IveGotAPlan
  | IveGotAPlan2' IveGotAPlan2
  | LetMeHandleThis' LetMeHandleThis
  | LookWhatIFound' LookWhatIFound
  | Lucky' Lucky
  | Lucky2' Lucky2
  | MindOverMatter' MindOverMatter
  | MindWipe1' MindWipe1
  | MindWipe3' MindWipe3
  | OnTheLam' OnTheLam
  | Oops' Oops
  | SearchForTheTruth' SearchForTheTruth
  | SecondWind' SecondWind
  | SeekingAnswers' SeekingAnswers
  | Shortcut' Shortcut
  | SneakAttack' SneakAttack
  | SureGamble3' SureGamble3
  | Taunt' Taunt
  | Taunt2' Taunt2
  | Taunt3' Taunt3
  | Teamwork' Teamwork
  | ThinkOnYourFeet' ThinkOnYourFeet
  | WardOfProtection' WardOfProtection
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
deriving anyclass instance HasCount ClueCount env InvestigatorId => HasModifiersFor env Event
deriving anyclass instance
  ( EventRunner env
  , HasSet FightableEnemyId env (InvestigatorId, Source)
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
  [ AstoundingRevelation' <$> astoundingRevelation
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
  , DelveTooDeep' <$> delveTooDeep
  , Dodge' <$> dodge
  , DrawnToTheFlame' <$> drawnToTheFlame
  , DynamiteBlast' <$> dynamiteBlast
  , DynamiteBlast2' <$> dynamiteBlast2
  , Elusive' <$> elusive
  , EmergencyAid' <$> emergencyAid
  , EmergencyCache' <$> emergencyCache
  , Evidence' <$> evidence
  , ExtraAmmunition1' <$> extraAmmunition1
  , FirstWatch' <$> firstWatch
  , Flare1' <$> flare1
  , HotStreak2' <$> hotStreak2
  , HotStreak4' <$> hotStreak4
  , IveGotAPlan' <$> iveGotAPlan
  , IveGotAPlan2' <$> iveGotAPlan2
  , LetMeHandleThis' <$> letMeHandleThis
  , LookWhatIFound' <$> lookWhatIFound
  , Lucky' <$> lucky
  , Lucky2' <$> lucky2
  , MindOverMatter' <$> mindOverMatter
  , MindWipe1' <$> mindWipe1
  , MindWipe3' <$> mindWipe3
  , OnTheLam' <$> onTheLam
  , Oops' <$> oops
  , SearchForTheTruth' <$> searchForTheTruth
  , SecondWind' <$> secondWind
  , SeekingAnswers' <$> seekingAnswers
  , Shortcut' <$> shortcut
  , SneakAttack' <$> sneakAttack
  , SureGamble3' <$> sureGamble3
  , Taunt' <$> taunt
  , Taunt2' <$> taunt2
  , Taunt3' <$> taunt3
  , Teamwork' <$> teamwork
  , ThinkOnYourFeet' <$> thinkOnYourFeet
  , WardOfProtection' <$> wardOfProtection
  , WillToSurvive3' <$> willToSurvive3
  , WorkingAHunch' <$> workingAHunch
  ]

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs
