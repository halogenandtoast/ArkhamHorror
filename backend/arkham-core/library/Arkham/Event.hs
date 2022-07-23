module Arkham.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Events
import Arkham.Event.Runner
import Arkham.Id
import Data.Typeable

data Event = forall a. IsEvent a => Event a

instance Eq Event where
  (Event (a :: a)) == (Event (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance HasCardCode Event where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Event where
  getAbilities (Event a) = getAbilities a

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a
  overAttrs f (Event a) = Event $ overAttrs f a

instance TargetEntity Event where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Event where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Event where
  toCardId = toCardId . toAttrs
  toCard e = lookupCard (eventOriginalCardCode . toAttrs $ e) (toCardId e)
  toCardOwner = toCardOwner . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode =
  fromJustNote ("Unknown event: " <> show cardCode) $ lookup cardCode allEvents

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs

instance FromJSON Event where
  parseJSON v = flip (withObject "Event") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      "01010" -> Event . OnTheLam <$> parseJSON v
      "01013" -> Event . DarkMemory <$> parseJSON v
      "01022" -> Event . Evidence <$> parseJSON v
      "01023" -> Event . Dodge <$> parseJSON v
      "01024" -> Event . DynamiteBlast <$> parseJSON v
      "01026" -> Event . ExtraAmmunition1 <$> parseJSON v
      "01036" -> Event . MindOverMatter <$> parseJSON v
      "01037" -> Event . WorkingAHunch <$> parseJSON v
      "01038" -> Event . Barricade <$> parseJSON v
      "01043" -> Event . CrypticResearch4 <$> parseJSON v
      "01050" -> Event . Elusive <$> parseJSON v
      "01051" -> Event . Backstab <$> parseJSON v
      "01052" -> Event . SneakAttack <$> parseJSON v
      "01056" -> Event . SureGamble3 <$> parseJSON v
      "01057" -> Event . HotStreak4 <$> parseJSON v
      "01064" -> Event . DrawnToTheFlame <$> parseJSON v
      "01065" -> Event . WardOfProtection <$> parseJSON v
      "01066" -> Event . BlindingLight <$> parseJSON v
      "01068" -> Event . MindWipe1 <$> parseJSON v
      "01069" -> Event . BlindingLight2 <$> parseJSON v
      "01078" -> Event . CunningDistraction <$> parseJSON v
      "01079" -> Event . LookWhatIFound <$> parseJSON v
      "01080" -> Event . Lucky <$> parseJSON v
      "01083" -> Event . CloseCall2 <$> parseJSON v
      "01084" -> Event . Lucky2 <$> parseJSON v
      "01085" -> Event . WillToSurvive3 <$> parseJSON v
      "01088" -> Event . EmergencyCache <$> parseJSON v
      "02008" -> Event . SearchForTheTruth <$> parseJSON v
      "02017" -> Event . Taunt <$> parseJSON v
      "02018" -> Event . Teamwork <$> parseJSON v
      "02019" -> Event . Taunt2 <$> parseJSON v
      "02022" -> Event . Shortcut <$> parseJSON v
      "02023" -> Event . SeekingAnswers <$> parseJSON v
      "02025" -> Event . ThinkOnYourFeet <$> parseJSON v
      "02031" -> Event . BindMonster2 <$> parseJSON v
      "02034" -> Event . BaitAndSwitch <$> parseJSON v
      "02105" -> Event . EmergencyAid <$> parseJSON v
      "02107" -> Event . IveGotAPlan <$> parseJSON v
      "02109" -> Event . Contraband <$> parseJSON v
      "02111" -> Event . DelveTooDeep <$> parseJSON v
      "02113" -> Event . Oops <$> parseJSON v
      "02115" -> Event . Flare1 <$> parseJSON v
      "02148" -> Event . StandTogether3 <$> parseJSON v
      "02151" -> Event . ImOuttaHere <$> parseJSON v
      "02153" -> Event . HypnoticGaze <$> parseJSON v
      "02156" -> Event . Lure1 <$> parseJSON v
      "02184" -> Event . PreparedForTheWorst <$> parseJSON v
      "02186" -> Event . PreposterousSketches <$> parseJSON v
      "02194" -> Event . EmergencyCache2 <$> parseJSON v
      "02225" -> Event . IfItBleeds <$> parseJSON v
      "02228" -> Event . ExposeWeakness1 <$> parseJSON v
      "02261" -> Event . IveHadWorse4 <$> parseJSON v
      "02266" -> Event . AceInTheHole3 <$> parseJSON v
      "02267" -> Event . MoonlightRitual <$> parseJSON v
      "02270" -> Event . AChanceEncounter <$> parseJSON v
      "02273" -> Event . MomentOfRespite3 <$> parseJSON v
      "02300" -> Event . MonsterSlayer5 <$> parseJSON v
      "02303" -> Event . DecipheredReality5 <$> parseJSON v
      "02307" -> Event . WardOfProtection5 <$> parseJSON v
      "03012" -> Event . ThePaintedWorld <$> parseJSON v
      "03016" -> Event . BuryThemDeep <$> parseJSON v
      "03018" -> Event . Improvisation <$> parseJSON v
      "03022" -> Event . LetMeHandleThis <$> parseJSON v
      "03023" -> Event . EverVigilant1 <$> parseJSON v
      "03026" -> Event . NoStoneUnturned <$> parseJSON v
      "03029" -> Event . SleightOfHand <$> parseJSON v
      "03030" -> Event . DaringManeuver <$> parseJSON v
      "03033" -> Event . UncageTheSoul <$> parseJSON v
      "03034" -> Event . AstralTravel <$> parseJSON v
      "03038" -> Event . HidingSpot <$> parseJSON v
      "03106" -> Event . HeroicRescue <$> parseJSON v
      "03108" -> Event . AnatomicalDiagrams <$> parseJSON v
      "03148" -> Event . Ambush1 <$> parseJSON v
      "03150" -> Event . Forewarned1 <$> parseJSON v
      "03152" -> Event . SneakAttack2 <$> parseJSON v
      "03153" -> Event . StormOfSpirits <$> parseJSON v
      "03155" -> Event . FightOrFlight <$> parseJSON v
      "03156" -> Event . ATestOfWill1 <$> parseJSON v
      "03157" -> Event . DevilsLuck <$> parseJSON v
      "03158" -> Event . CallingInFavors <$> parseJSON v
      "03189" -> Event . IllSeeYouInHell <$> parseJSON v
      "03191" -> Event . LogicalReasoning <$> parseJSON v
      "03194" -> Event . CheapShot <$> parseJSON v
      "03196" -> Event . QuantumFlux <$> parseJSON v
      "03197" -> Event . Recharge2 <$> parseJSON v
      "03199" -> Event . SnareTrap2 <$> parseJSON v
      "03229" -> Event . ManoAMano1 <$> parseJSON v
      "03232" -> Event . Shortcut2 <$> parseJSON v
      "03237" -> Event . Waylay <$> parseJSON v
      "03238" -> Event . AChanceEncounter2 <$> parseJSON v
      "03239" -> Event . EmergencyCache3 <$> parseJSON v
      "03263" -> Event . OnTheHunt <$> parseJSON v
      "03265" -> Event . Guidance <$> parseJSON v
      "03267" -> Event . NarrowEscape <$> parseJSON v
      "03270" -> Event . WardOfProtection2 <$> parseJSON v
      "03273" -> Event . TrueSurvivor3 <$> parseJSON v
      "03304" -> Event . EatLead2 <$> parseJSON v
      "03306" -> Event . EideticMemory3 <$> parseJSON v
      "03307" -> Event . NoStoneUnturned5 <$> parseJSON v
      "03310" -> Event . CheatDeath5 <$> parseJSON v
      "03311" -> Event . TimeWarp2 <$> parseJSON v
      "03314" -> Event . Infighting3 <$> parseJSON v
      "04010" -> Event . SmuggledGoods <$> parseJSON v
      "04019" -> Event . Trusted <$> parseJSON v
      "04020" -> Event . Reliable1 <$> parseJSON v
      "04024" -> Event . UnearthTheAncients <$> parseJSON v
      "04027" -> Event . Eavesdrop <$> parseJSON v
      "04028" -> Event . YouHandleThisOne <$> parseJSON v
      "04032" -> Event . DarkProphecy <$> parseJSON v
      "04033" -> Event . ImprovisedWeapon <$> parseJSON v
      "04034" -> Event . DumbLuck <$> parseJSON v
      "04038" -> Event . DarkPact <$> parseJSON v
      "04149" -> Event . SecondWind <$> parseJSON v
      "04200" -> Event . LiveAndLearn <$> parseJSON v
      "04272" -> Event . WingingIt <$> parseJSON v
      "05317" -> Event . BloodRite <$> parseJSON v
      "06023" -> Event . AstoundingRevelation <$> parseJSON v
      "06110" -> Event . FirstWatch <$> parseJSON v
      "06165" -> Event . ScroungeForSupplies <$> parseJSON v
      "50002" -> Event . DynamiteBlast2 <$> parseJSON v
      "50004" -> Event . Barricade3 <$> parseJSON v
      "50006" -> Event . HotStreak2 <$> parseJSON v
      "50008" -> Event . MindWipe3 <$> parseJSON v
      "51003" -> Event . PreposterousSketches2 <$> parseJSON v
      "51005" -> Event . Contraband2 <$> parseJSON v
      "60111" -> Event . CleanThemOut <$> parseJSON v
      "60112" -> Event . Counterpunch <$> parseJSON v
      "60114" -> Event . GetOverHere <$> parseJSON v
      "60115" -> Event . Glory <$> parseJSON v
      "60116" -> Event . MonsterSlayer <$> parseJSON v
      "60117" -> Event . OneTwoPunch <$> parseJSON v
      "60130" -> Event . Taunt3 <$> parseJSON v
      "60225" -> Event . IveGotAPlan2 <$> parseJSON v
      "60515" -> Event . GritYourTeeth <$> parseJSON v
      "60524" -> Event . LookWhatIFound2 <$> parseJSON v
      _ -> error "invalid event"

allEvents :: HashMap CardCode (InvestigatorId -> EventId -> Event)
allEvents = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  [ Event <$> onTheLam
  , Event <$> darkMemory
  , Event <$> evidence
  , Event <$> dodge
  , Event <$> dynamiteBlast
  , Event <$> extraAmmunition1
  , Event <$> mindOverMatter
  , Event <$> workingAHunch
  , Event <$> barricade
  , Event <$> crypticResearch4
  , Event <$> elusive
  , Event <$> backstab
  , Event <$> sneakAttack
  , Event <$> sureGamble3
  , Event <$> hotStreak4
  , Event <$> drawnToTheFlame
  , Event <$> wardOfProtection
  , Event <$> blindingLight
  , Event <$> mindWipe1
  , Event <$> blindingLight2
  , Event <$> cunningDistraction
  , Event <$> lookWhatIFound
  , Event <$> lucky
  , Event <$> closeCall2
  , Event <$> lucky2
  , Event <$> willToSurvive3
  , Event <$> emergencyCache
  , Event <$> searchForTheTruth
  , Event <$> taunt
  , Event <$> teamwork
  , Event <$> taunt2
  , Event <$> shortcut
  , Event <$> seekingAnswers
  , Event <$> thinkOnYourFeet
  , Event <$> bindMonster2
  , Event <$> baitAndSwitch
  , Event <$> emergencyAid
  , Event <$> iveGotAPlan
  , Event <$> contraband
  , Event <$> delveTooDeep
  , Event <$> oops
  , Event <$> flare1
  , Event <$> standTogether3
  , Event <$> imOuttaHere
  , Event <$> hypnoticGaze
  , Event <$> lure1
  , Event <$> preparedForTheWorst
  , Event <$> preposterousSketches
  , Event <$> emergencyCache2
  , Event <$> ifItBleeds
  , Event <$> exposeWeakness1
  , Event <$> iveHadWorse4
  , Event <$> aceInTheHole3
  , Event <$> moonlightRitual
  , Event <$> aChanceEncounter
  , Event <$> momentOfRespite3
  , Event <$> monsterSlayer5
  , Event <$> decipheredReality5
  , Event <$> wardOfProtection5
  , Event <$> thePaintedWorld
  , Event <$> buryThemDeep
  , Event <$> improvisation
  , Event <$> letMeHandleThis
  , Event <$> everVigilant1
  , Event <$> noStoneUnturned
  , Event <$> sleightOfHand
  , Event <$> daringManeuver
  , Event <$> uncageTheSoul
  , Event <$> astralTravel
  , Event <$> hidingSpot
  , Event <$> heroicRescue
  , Event <$> anatomicalDiagrams
  , Event <$> ambush1
  , Event <$> forewarned1
  , Event <$> sneakAttack2
  , Event <$> stormOfSpirits
  , Event <$> fightOrFlight
  , Event <$> aTestOfWill1
  , Event <$> devilsLuck
  , Event <$> callingInFavors
  , Event <$> illSeeYouInHell
  , Event <$> logicalReasoning
  , Event <$> cheapShot
  , Event <$> quantumFlux
  , Event <$> recharge2
  , Event <$> snareTrap2
  , Event <$> manoAMano1
  , Event <$> shortcut2
  , Event <$> waylay
  , Event <$> aChanceEncounter2
  , Event <$> emergencyCache3
  , Event <$> onTheHunt
  , Event <$> guidance
  , Event <$> narrowEscape
  , Event <$> wardOfProtection2
  , Event <$> trueSurvivor3
  , Event <$> eatLead2
  , Event <$> eideticMemory3
  , Event <$> noStoneUnturned5
  , Event <$> cheatDeath5
  , Event <$> timeWarp2
  , Event <$> infighting3
  , Event <$> smuggledGoods
  , Event <$> trusted
  , Event <$> reliable1
  , Event <$> unearthTheAncients
  , Event <$> eavesdrop
  , Event <$> youHandleThisOne
  , Event <$> darkProphecy
  , Event <$> improvisedWeapon
  , Event <$> dumbLuck
  , Event <$> darkPact
  , Event <$> secondWind
  , Event <$> liveAndLearn
  , Event <$> wingingIt
  , Event <$> bloodRite
  , Event <$> astoundingRevelation
  , Event <$> firstWatch
  , Event <$> scroungeForSupplies
  , Event <$> dynamiteBlast2
  , Event <$> barricade3
  , Event <$> hotStreak2
  , Event <$> mindWipe3
  , Event <$> preposterousSketches2
  , Event <$> contraband2
  , Event <$> cleanThemOut
  , Event <$> counterpunch
  , Event <$> getOverHere
  , Event <$> glory
  , Event <$> monsterSlayer
  , Event <$> oneTwoPunch
  , Event <$> taunt3
  , Event <$> iveGotAPlan2
  , Event <$> gritYourTeeth
  , Event <$> lookWhatIFound2
  ]
