module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Data.Typeable

data Treachery = forall a. IsTreachery a => Treachery a

instance Eq Treachery where
  (Treachery (a :: a)) == (Treachery (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Treachery where
  show (Treachery a) = show a

instance ToJSON Treachery where
  toJSON (Treachery a) = toJSON a

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Treachery where
  getAbilities (Treachery a) = getAbilities a

instance RunMessage Treachery where
  runMessage msg (Treachery a) = Treachery <$> runMessage msg a

instance HasModifiersFor Treachery where
  getModifiersFor source target (Treachery a) = getModifiersFor source target a

instance HasCardCode Treachery where
  toCardCode = toCardCode . toAttrs

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs
  toId = toId . toAttrs
  toAttrs (Treachery a) = toAttrs a
  overAttrs f (Treachery a) = Treachery $ overAttrs f a

instance TargetEntity Treachery where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Treachery where
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

instance FromJSON Treachery where
  parseJSON v = flip (withObject "Treachery") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      -- Night of the Zealot
      -- signature
      "01007" -> Treachery . CoverUp <$> parseJSON v
      "01011" -> Treachery . HospitalDebts <$> parseJSON v
      "01015" -> Treachery . AbandonedAndAlone <$> parseJSON v
      -- weakness
      "01096" -> Treachery . Amnesia <$> parseJSON v
      "01097" -> Treachery . Paranoia <$> parseJSON v
      "01098" -> Treachery . Haunted <$> parseJSON v
      "01099" -> Treachery . Psychosis <$> parseJSON v
      "01100" -> Treachery . Hypochondria <$> parseJSON v
      -- The Midnight Masks
      "01135" -> Treachery . HuntingShadow <$> parseJSON v
      "01136" -> Treachery . FalseLead <$> parseJSON v
      -- The Devourer Below
      "01158" -> Treachery . UmordhothsWrath <$> parseJSON v
      -- Ghouls
      "01162" -> Treachery . GraspingHands <$> parseJSON v
      -- Striking Fear
      "01163" -> Treachery . RottingRemains <$> parseJSON v
      "01164" -> Treachery . FrozenInFear <$> parseJSON v
      "01165" -> Treachery . DissonantVoices <$> parseJSON v
      -- Ancient Evils
      "01166" -> Treachery . AncientEvils <$> parseJSON v
      -- Chilling Colds
      "01167" -> Treachery . CryptChill <$> parseJSON v
      "01168" -> Treachery . ObscuringFog <$> parseJSON v
      -- Dark Cult
      "01171" -> Treachery . MysteriousChanting <$> parseJSON v
      -- Nightgaunts
      "01173" -> Treachery . OnWingsOfDarkness <$> parseJSON v
      -- Locked Doors
      "01174" -> Treachery . LockedDoor <$> parseJSON v
      -- Agents of Hastur
      "01176" -> Treachery . TheYellowSign <$> parseJSON v
      -- Agents of Yog Sothoth
      "01178" -> Treachery . OfferOfPower <$> parseJSON v
      -- Agents of Cthulhu
      "01182" -> Treachery . DreamsOfRlyeh <$> parseJSON v
      -- The Dunwich Legacy
      -- signature
      "02007" -> Treachery . SmiteTheWicked <$> parseJSON v
      "02009" -> Treachery . RexsCurse <$> parseJSON v
      "02011" -> Treachery . SearchingForIzzie <$> parseJSON v
      "02013" -> Treachery . FinalRhapsody <$> parseJSON v
      "02015" -> Treachery . WrackedByNightmares <$> parseJSON v
      -- weakness
      "02037" -> Treachery . Indebted <$> parseJSON v
      "02038" -> Treachery . InternalInjury <$> parseJSON v
      "02039" -> Treachery . Chronophobia <$> parseJSON v
      -- The House Always Wins
      "02081" -> Treachery . SomethingInTheDrinks <$> parseJSON v
      "02082" -> Treachery . ArousingSuspicions <$> parseJSON v
      -- Sorcery
      "02083" -> Treachery . VisionsOfFuturesPast <$> parseJSON v
      "02084" -> Treachery . BeyondTheVeil <$> parseJSON v
      -- Bishop's Thralls
      "02085" -> Treachery . LightOfAforgomon <$> parseJSON v
      -- Dunwich
      "02088" -> Treachery . UnhallowedCountry <$> parseJSON v
      "02089" -> Treachery . SordidAndSilent <$> parseJSON v
      -- Whippoorwill
      "02091" -> Treachery . EagerForDeath <$> parseJSON v
      -- Bad Luck
      "02092" -> Treachery . CursedLuck <$> parseJSON v
      "02093" -> Treachery . TwistOfFate <$> parseJSON v
      -- Beast Thralls
      "02096" -> Treachery . AlteredBeast <$> parseJSON v
      -- Naomi's Crew
      "02099" -> Treachery . HuntedDown <$> parseJSON v
      -- The Beyond
      "02100" -> Treachery . PushedIntoTheBeyond <$> parseJSON v
      "02101" -> Treachery . TerrorFromBeyond <$> parseJSON v
      "02102" -> Treachery . ArcaneBarrier <$> parseJSON v
      -- The Miskatonic Museum
      "02142" -> Treachery . ShadowSpawned <$> parseJSON v
      "02143" -> Treachery . StalkedInTheDark <$> parseJSON v
      "02144" -> Treachery . PassageIntoTheVeil <$> parseJSON v
      "02145" -> Treachery . EphemeralExhibits <$> parseJSON v
      "02146" -> Treachery . SlitheringBehindYou <$> parseJSON v
      -- The Essex County Express
      "02178" -> Treachery . AcrossSpaceAndTime <$> parseJSON v
      "02180" -> Treachery . ClawsOfSteam <$> parseJSON v
      "02181" -> Treachery . BrokenRails <$> parseJSON v
      -- Blood on the Altar
      "02220" -> Treachery . Kidnapped <$> parseJSON v
      "02221" -> Treachery . PsychopompsSong <$> parseJSON v
      "02222" -> Treachery . StrangeSigns <$> parseJSON v
      "02223" -> Treachery . RottingRemainsBloodOnTheAltar <$> parseJSON v
      -- Undimensioned and Unseen
      "02256" -> Treachery . ToweringBeasts <$> parseJSON v
      "02257" -> Treachery . RuinAndDestruction <$> parseJSON v
      "02258" -> Treachery . AttractingAttention <$> parseJSON v
      "02259" -> Treachery . TheCreaturesTracks <$> parseJSON v
      -- Where Doom Awaits
      "02296" -> Treachery . RitesHowled <$> parseJSON v
      "02297" -> Treachery . SpacesBetween <$> parseJSON v
      "02298" -> Treachery . VortexOfTime <$> parseJSON v
      -- Lost in Time and Space
      "02331" -> Treachery . CollapsingReality <$> parseJSON v
      "02332" -> Treachery . Wormhole <$> parseJSON v
      "02333" -> Treachery . VastExpanse <$> parseJSON v
      -- The Path to Carcosa
      -- signature
      "03008" -> Treachery . ShellShock <$> parseJSON v
      "03013" -> Treachery . StarsOfHyades <$> parseJSON v
      "03015" -> Treachery . AngeredSpirits <$> parseJSON v
      "03019" -> Treachery . CrisisOfIdentity <$> parseJSON v
      -- weakness
      "03040" -> Treachery . Overzealous <$> parseJSON v
      "03041" -> Treachery . DrawingTheSign <$> parseJSON v
      -- The Last King
      "03082" -> Treachery . FineDining <$> parseJSON v
      "03083" -> Treachery . ToughCrowd <$> parseJSON v
      -- Delusions
      "03084a" -> Treachery . WhispersInYourHeadDismay <$> parseJSON v
      "03084b" -> Treachery . WhispersInYourHeadDread <$> parseJSON v
      "03084c" -> Treachery . WhispersInYourHeadAnxiety <$> parseJSON v
      "03084d" -> Treachery . WhispersInYourHeadDoubt <$> parseJSON v
      "03085" -> Treachery . DescentIntoMadness <$> parseJSON v
      -- Byakhee
      "03087" -> Treachery . HuntedByByakhee <$> parseJSON v
      -- Evil Portants
      "03090" -> Treachery . BlackStarsRise <$> parseJSON v
      "03091" -> Treachery . SpiresOfCarcosa <$> parseJSON v
      "03092" -> Treachery . TwistedToHisWill <$> parseJSON v
      -- Hauntings
      "03094" -> Treachery . SpiritsTorment <$> parseJSON v
      -- Hastur's Gift
      "03097" -> Treachery . DanceOfTheYellowKing <$> parseJSON v
      -- Cult of the Yellow Sign
      "03100" -> Treachery . TheKingsEdict <$> parseJSON v
      -- Decay and Filth
      "03101" -> Treachery . OozeAndFilth <$> parseJSON v
      "03102" -> Treachery . Corrosion <$> parseJSON v
      -- The Stranger
      "03104" -> Treachery . MarkedByTheSign <$> parseJSON v
      "03105" -> Treachery . ThePaleMaskBeckons <$> parseJSON v
      -- Echoes of the Past
      "03145" -> Treachery . LedAstray <$> parseJSON v
      "03146" -> Treachery . TheCultsSearch <$> parseJSON v
      -- The Unspeakable Oath
      "03185" -> Treachery . Straitjacket <$> parseJSON v
      "03186" -> Treachery . GiftOfMadnessPity <$> parseJSON v
      "03187" -> Treachery . GiftOfMadnessMisery <$> parseJSON v
      "03188" -> Treachery . WallsClosingIn <$> parseJSON v
      -- A Phantom of Truth
      "03223" -> Treachery . TwinSuns <$> parseJSON v
      "03224" -> Treachery . DeadlyFate <$> parseJSON v
      "03225" -> Treachery . TorturousChords <$> parseJSON v
      "03226" -> Treachery . FrozenInFearAPhantomOfTruth <$> parseJSON v
      "03227" -> Treachery . LostSoul <$> parseJSON v
      -- The Pallid Mask
      "03260" -> Treachery . EyesInTheWalls <$> parseJSON v
      "03261" -> Treachery . TheShadowBehindYou <$> parseJSON v
      "03262" -> Treachery . ThePitBelow <$> parseJSON v
      -- Black Stars Rise
      "03302" -> Treachery . CrashingFloods <$> parseJSON v
      "03303" -> Treachery . WorldsMerge <$> parseJSON v
      -- Dim Carcosa
      "03337" -> Treachery . DismalCurse <$> parseJSON v
      "03338" -> Treachery . RealmOfMadness <$> parseJSON v
      "03339" -> Treachery . TheFinalAct <$> parseJSON v
      "03340" -> Treachery . PossessionTraitorous <$> parseJSON v
      "03341" -> Treachery . PossessionTorturous <$> parseJSON v
      "03342" -> Treachery . PossessionMurderous <$> parseJSON v
      -- Forgotten Age
      -- signature
      "04007" -> Treachery . BoughtInBlood <$> parseJSON v
      "04009" -> Treachery . CallOfTheUnknown <$> parseJSON v
      "04012" -> Treachery . CaughtRedHanded <$> parseJSON v
      -- Edge of the Earth
      -- signature
      "08006" -> Treachery . TheHarbinger <$> parseJSON v
      -- Return to the Night of the Zealot
      -- Return to the Gathering
      "50024" -> Treachery . TheZealotsSeal <$> parseJSON v
      -- Return to the Midnight Masks
      "50031" -> Treachery . MaskedHorrors <$> parseJSON v
      -- Return to the Devourer Below
      "50032b" -> Treachery . VaultOfEarthlyDemise <$> parseJSON v
      "50037" -> Treachery . UmordhothsHunger <$> parseJSON v
      -- Ghouls of Umordhoth
      "50040" -> Treachery . ChillFromBelow <$> parseJSON v
      -- The Devourer's Cult
      "50043" -> Treachery . ChillFromBelow <$> parseJSON v
      -- Stella Clark
      "60503" -> Treachery . CalledByTheMists <$> parseJSON v
      "60504" -> Treachery . Atychiphobia <$> parseJSON v
      -- Curse of the Rougarou
      "81024" -> Treachery . CursedSwamp <$> parseJSON v
      "81025" -> Treachery . SpectralMist <$> parseJSON v
      "81026" -> Treachery . DraggedUnder <$> parseJSON v
      "81027" -> Treachery . RipplesOnTheSurface <$> parseJSON v
      "81029" -> Treachery . CurseOfTheRougarou <$> parseJSON v
      "81034" -> Treachery . OnTheProwl <$> parseJSON v
      "81035" -> Treachery . BeastOfTheBayou <$> parseJSON v
      "81036" -> Treachery . InsatiableBloodlust <$> parseJSON v
      -- Carnevale of Horror
      "82031" -> Treachery . MassHysteria <$> parseJSON v
      "82032" -> Treachery . LostInVenice <$> parseJSON v
      "82033" -> Treachery . WatchersGaze <$> parseJSON v
      "82034" -> Treachery . ChaosInTheWater <$> parseJSON v
      "82035" -> Treachery . Mesmerize <$> parseJSON v
      "82036" -> Treachery . Abduction <$> parseJSON v
      "82037" -> Treachery . AcridMiasma <$> parseJSON v
      _ -> error "Invalid card code"

allTreacheries :: HashMap CardCode (InvestigatorId -> TreacheryId -> Treachery)
allTreacheries = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  [ -- Night of the Zealot
  -- signature
    Treachery <$> coverUp
  , Treachery <$> hospitalDebts
  , Treachery <$> abandonedAndAlone
  -- weakness
  , Treachery <$> amnesia
  , Treachery <$> paranoia
  , Treachery <$> haunted
  , Treachery <$> psychosis
  , Treachery <$> hypochondria
  -- The Midnight Masks
  , Treachery <$> huntingShadow
  , Treachery <$> falseLead
  -- The Devourer Below
  , Treachery <$> umordhothsWrath
  -- Ghouls
  , Treachery <$> graspingHands
  -- Striking Fear
  , Treachery <$> rottingRemains
  , Treachery <$> frozenInFear
  , Treachery <$> dissonantVoices
  -- Ancient Evils
  , Treachery <$> ancientEvils
  -- Chilling Colds
  , Treachery <$> cryptChill
  , Treachery <$> obscuringFog
  -- Dark Cult
  , Treachery <$> mysteriousChanting
  -- Nightgaunts
  , Treachery <$> onWingsOfDarkness
  -- Locked Doors
  , Treachery <$> lockedDoor
  -- Agents of Hastur
  , Treachery <$> theYellowSign
  -- Agents of Yog Sothoth
  , Treachery <$> offerOfPower
  -- Agents of Cthulhu
  , Treachery <$> dreamsOfRlyeh
  -- The Dunwich Legacy
  -- signature
  , Treachery <$> smiteTheWicked
  , Treachery <$> rexsCurse
  , Treachery <$> searchingForIzzie
  , Treachery <$> finalRhapsody
  , Treachery <$> wrackedByNightmares
  -- weakness
  , Treachery <$> indebted
  , Treachery <$> internalInjury
  , Treachery <$> chronophobia
  -- The House Always Wins
  , Treachery <$> somethingInTheDrinks
  , Treachery <$> arousingSuspicions
  -- Sorcery
  , Treachery <$> visionsOfFuturesPast
  , Treachery <$> beyondTheVeil
  -- Bishop's Thralls
  , Treachery <$> lightOfAforgomon
  -- Dunwich
  , Treachery <$> unhallowedCountry
  , Treachery <$> sordidAndSilent
  -- Whippoorwill
  , Treachery <$> eagerForDeath
  -- Bad Luck
  , Treachery <$> cursedLuck
  , Treachery <$> twistOfFate
  -- Beast Thralls
  , Treachery <$> alteredBeast
  -- Naomi's Crew
  , Treachery <$> huntedDown
  -- The Beyond
  , Treachery <$> pushedIntoTheBeyond
  , Treachery <$> terrorFromBeyond
  , Treachery <$> arcaneBarrier
  -- The Miskatonic Museum
  , Treachery <$> shadowSpawned
  , Treachery <$> stalkedInTheDark
  , Treachery <$> passageIntoTheVeil
  , Treachery <$> ephemeralExhibits
  , Treachery <$> slitheringBehindYou
  -- The Essex County Express
  , Treachery <$> acrossSpaceAndTime
  , Treachery <$> clawsOfSteam
  , Treachery <$> brokenRails
  -- Blood on the Altar
  , Treachery <$> kidnapped
  , Treachery <$> psychopompsSong
  , Treachery <$> strangeSigns
  , Treachery <$> rottingRemainsBloodOnTheAltar
  -- Undimensioned and Unseen
  , Treachery <$> toweringBeasts
  , Treachery <$> ruinAndDestruction
  , Treachery <$> attractingAttention
  , Treachery <$> theCreaturesTracks
  -- Where Doom Awaits
  , Treachery <$> ritesHowled
  , Treachery <$> spacesBetween
  , Treachery <$> vortexOfTime
  -- Lost in Time and Space
  , Treachery <$> collapsingReality
  , Treachery <$> wormhole
  , Treachery <$> vastExpanse
  -- The Path to Carcosa
  -- signature
  , Treachery <$> shellShock
  , Treachery <$> starsOfHyades
  , Treachery <$> angeredSpirits
  , Treachery <$> crisisOfIdentity
  -- weakness
  , Treachery <$> overzealous
  , Treachery <$> drawingTheSign
  -- The Last King
  , Treachery <$> fineDining
  , Treachery <$> toughCrowd
  -- Delusions
  , Treachery <$> whispersInYourHeadDismay
  , Treachery <$> whispersInYourHeadDread
  , Treachery <$> whispersInYourHeadAnxiety
  , Treachery <$> whispersInYourHeadDoubt
  , Treachery <$> descentIntoMadness
  -- Byakhee
  , Treachery <$> huntedByByakhee
  -- Evil Portants
  , Treachery <$> blackStarsRise
  , Treachery <$> spiresOfCarcosa
  , Treachery <$> twistedToHisWill
  -- Hauntings
  , Treachery <$> spiritsTorment
  -- Hastur's Gift
  , Treachery <$> danceOfTheYellowKing
  -- Cult of the Yellow Sign
  , Treachery <$> theKingsEdict
  -- Decay and Filth
  , Treachery <$> oozeAndFilth
  , Treachery <$> corrosion
  -- The Stranger
  , Treachery <$> markedByTheSign
  , Treachery <$> thePaleMaskBeckons
  -- Echoes of the Past
  , Treachery <$> ledAstray
  , Treachery <$> theCultsSearch
  -- The Unspeakable Oath
  , Treachery <$> straitjacket
  , Treachery <$> giftOfMadnessPity
  , Treachery <$> giftOfMadnessMisery
  , Treachery <$> wallsClosingIn
  -- A Phantom of Truth
  , Treachery <$> twinSuns
  , Treachery <$> deadlyFate
  , Treachery <$> torturousChords
  , Treachery <$> frozenInFearAPhantomOfTruth
  , Treachery <$> lostSoul
  -- The Pallid Mask
  , Treachery <$> eyesInTheWalls
  , Treachery <$> theShadowBehindYou
  , Treachery <$> thePitBelow
  -- Black Stars Rise
  , Treachery <$> crashingFloods
  , Treachery <$> worldsMerge
  -- Dim Carcosa
  , Treachery <$> dismalCurse
  , Treachery <$> realmOfMadness
  , Treachery <$> theFinalAct
  , Treachery <$> possessionTraitorous
  , Treachery <$> possessionTorturous
  , Treachery <$> possessionMurderous
  -- Forgotten Age
  -- signature
  , Treachery <$> boughtInBlood
  , Treachery <$> callOfTheUnknown
  , Treachery <$> caughtRedHanded
  -- Edge of the Earth
  -- signature
  , Treachery <$> theHarbinger
  -- Return to the Night of the Zealot
  -- Return to the Gathering
  , Treachery <$> theZealotsSeal
  -- Return to the Midnight Masks
  , Treachery <$> maskedHorrors
  -- Return to the Devourer Below
  , Treachery <$> vaultOfEarthlyDemise
  , Treachery <$> umordhothsHunger
  -- Ghouls of Umordhoth
  , Treachery <$> chillFromBelow
  -- The Devourer's Cult
  , Treachery <$> chillFromBelow
  -- Stella Clark
  , Treachery <$> calledByTheMists
  , Treachery <$> atychiphobia
  -- Curse of the Rougarou
  , Treachery <$> cursedSwamp
  , Treachery <$> spectralMist
  , Treachery <$> draggedUnder
  , Treachery <$> ripplesOnTheSurface
  , Treachery <$> curseOfTheRougarou
  , Treachery <$> onTheProwl
  , Treachery <$> beastOfTheBayou
  , Treachery <$> insatiableBloodlust
  -- Carnevale of Horror
  , Treachery <$> massHysteria
  , Treachery <$> lostInVenice
  , Treachery <$> watchersGaze
  , Treachery <$> chaosInTheWater
  , Treachery <$> mesmerize
  , Treachery <$> abduction
  , Treachery <$> acridMiasma
  ]
