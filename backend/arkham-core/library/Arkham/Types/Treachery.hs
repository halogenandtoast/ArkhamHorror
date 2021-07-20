module Arkham.Types.Treachery
  ( module Arkham.Types.Treachery
  ) where

import Arkham.Prelude

import Arkham.Types.AssetMatcher
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyMatcher
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait)
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards
import Arkham.Types.Treachery.Runner

createTreachery :: IsCard a => a -> InvestigatorId -> Treachery
createTreachery a iid =
  lookupTreachery (toCardCode a) iid (TreacheryId $ toCardId a)

data Treachery
  = BaseTreachery' BaseTreachery
  | AbandonedAndAlone' AbandonedAndAlone
  | Abduction' Abduction
  | AcridMiasma' AcridMiasma
  | AcrossSpaceAndTime' AcrossSpaceAndTime
  | AlteredBeast' AlteredBeast
  | Amnesia' Amnesia
  | AncientEvils' AncientEvils
  | ArcaneBarrier' ArcaneBarrier
  | ArousingSuspicions' ArousingSuspicions
  | AttractingAttention' AttractingAttention
  | Atychiphobia' Atychiphobia
  | BeastOfTheBayou' BeastOfTheBayou
  | BeyondTheVeil' BeyondTheVeil
  | BrokenRails' BrokenRails
  | ChaosInTheWater' ChaosInTheWater
  | ChillFromBelow' ChillFromBelow
  | Chronophobia' Chronophobia
  | ClawsOfSteam' ClawsOfSteam
  | CollapsingReality' CollapsingReality
  | CoverUp' CoverUp
  | CryptChill' CryptChill
  | CurseOfTheRougarou' CurseOfTheRougarou
  | CursedLuck' CursedLuck
  | CursedSwamp' CursedSwamp
  | DissonantVoices' DissonantVoices
  | DraggedUnder' DraggedUnder
  | DreamsOfRlyeh' DreamsOfRlyeh
  | EagerForDeath' EagerForDeath
  | EphemeralExhibits' EphemeralExhibits
  | FalseLead' FalseLead
  | FinalRhapsody' FinalRhapsody
  | FrozenInFear' FrozenInFear
  | GraspingHands' GraspingHands
  | Haunted' Haunted
  | HospitalDebts' HospitalDebts
  | HuntedDown' HuntedDown
  | HuntingShadow' HuntingShadow
  | Hypochondria' Hypochondria
  | Indebted' Indebted
  | InsatiableBloodlust' InsatiableBloodlust
  | InternalInjury' InternalInjury
  | Kidnapped' Kidnapped
  | LightOfAforgomon' LightOfAforgomon
  | LockedDoor' LockedDoor
  | LostInVenice' LostInVenice
  | MaskOfUmordhoth' MaskOfUmordhoth
  | MaskedHorrors' MaskedHorrors
  | MassHysteria' MassHysteria
  | Mesmerize' Mesmerize
  | MysteriousChanting' MysteriousChanting
  | ObscuringFog' ObscuringFog
  | OfferOfPower' OfferOfPower
  | OnTheProwl' OnTheProwl
  | OnWingsOfDarkness' OnWingsOfDarkness
  | Paranoia' Paranoia
  | PassageIntoTheVeil' PassageIntoTheVeil
  | PsychopompsSong' PsychopompsSong
  | Psychosis' Psychosis
  | PushedIntoTheBeyond' PushedIntoTheBeyond
  | RexsCurse' RexsCurse
  | RipplesOnTheSurface' RipplesOnTheSurface
  | RitesHowled' RitesHowled
  | RottingRemains' RottingRemains
  | RottingRemainsBloodOnTheAltar' RottingRemainsBloodOnTheAltar
  | RuinAndDestruction' RuinAndDestruction
  | SearchingForIzzie' SearchingForIzzie
  | ShadowSpawned' ShadowSpawned
  | SlitheringBehindYou' SlitheringBehindYou
  | SmiteTheWicked' SmiteTheWicked
  | SomethingInTheDrinks' SomethingInTheDrinks
  | SordidAndSilent' SordidAndSilent
  | SpacesBetween' SpacesBetween
  | SpectralMist' SpectralMist
  | StalkedInTheDark' StalkedInTheDark
  | StrangeSigns' StrangeSigns
  | TerrorFromBeyond' TerrorFromBeyond
  | TheCreaturesTracks' TheCreaturesTracks
  | TheYellowSign' TheYellowSign
  | TheZealotsSeal' TheZealotsSeal
  | ToweringBeasts' ToweringBeasts
  | TwistOfFate' TwistOfFate
  | UmordhothsHunger' UmordhothsHunger
  | UmordhothsWrath' UmordhothsWrath
  | UnhallowedCountry' UnhallowedCountry
  | VastExpanse' VastExpanse
  | VaultOfEarthlyDemise' VaultOfEarthlyDemise
  | VisionsOfFuturesPast' VisionsOfFuturesPast
  | VortexOfTime' VortexOfTime
  | WatchersGaze' WatchersGaze
  | Wormhole' Wormhole
  | WrackedByNightmares' WrackedByNightmares
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Treachery
deriving anyclass instance
  ( HasSet AssetId env AssetMatcher
  , GetCardDef env LocationId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet AssetId env (InvestigatorId, CardDef)
  , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
  , HasSet AssetId env (LocationId, AssetMatcher)
  , HasSet ClosestLocationId env (InvestigatorId, LocationMatcher)
  , HasSet EnemyId env EnemyMatcher
  , TreacheryRunner env
  )
  => RunMessage env Treachery

instance
  ( HasCount PlayerCount env ()
  , HasId LocationId env InvestigatorId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  , HasSet UniqueEnemyId env ()
  , HasCount ResourceCount env TreacheryId
  )
  => HasModifiersFor env Treachery where
  getModifiersFor = genericGetModifiersFor

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs

instance Named Treachery where
  toName = toName . toAttrs

instance TargetEntity Treachery where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Treachery where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Treachery where
  toCardId = toCardId . toAttrs

instance HasCount DoomCount env Treachery where
  getCount = pure . DoomCount . treacheryDoom . toAttrs

instance HasCount ResourceCount env Treachery where
  getCount = getCount . toAttrs

instance HasCount (Maybe ClueCount) env Treachery where
  getCount = pure . (ClueCount <$>) . treacheryClues . toAttrs

instance HasId (Maybe OwnerId) env Treachery where
  getId = pure . (OwnerId <$>) . treacheryOwner . toAttrs

lookupTreachery :: CardCode -> (InvestigatorId -> TreacheryId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

allTreacheries :: HashMap CardCode (InvestigatorId -> TreacheryId -> Treachery)
allTreacheries = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  [ CardBuilder
    { cbCardCode = "treachery"
    , cbCardBuilder = uncurry (baseTreachery "treachery")
    }
  , AbandonedAndAlone' <$> abandonedAndAlone
  , Abduction' <$> abduction
  , AcridMiasma' <$> acridMiasma
  , AcrossSpaceAndTime' <$> acrossSpaceAndTime
  , AlteredBeast' <$> alteredBeast
  , Amnesia' <$> amnesia
  , AncientEvils' <$> ancientEvils
  , ArcaneBarrier' <$> arcaneBarrier
  , ArousingSuspicions' <$> arousingSuspicions
  , AttractingAttention' <$> attractingAttention
  , Atychiphobia' <$> atychiphobia
  , BeastOfTheBayou' <$> beastOfTheBayou
  , BeyondTheVeil' <$> beyondTheVeil
  , BrokenRails' <$> brokenRails
  , ChaosInTheWater' <$> chaosInTheWater
  , ChillFromBelow' <$> chillFromBelow
  , Chronophobia' <$> chronophobia
  , ClawsOfSteam' <$> clawsOfSteam
  , CollapsingReality' <$> collapsingReality
  , CoverUp' <$> coverUp
  , CryptChill' <$> cryptChill
  , CurseOfTheRougarou' <$> curseOfTheRougarou
  , CursedLuck' <$> cursedLuck
  , CursedSwamp' <$> cursedSwamp
  , DissonantVoices' <$> dissonantVoices
  , DraggedUnder' <$> draggedUnder
  , DreamsOfRlyeh' <$> dreamsOfRlyeh
  , EagerForDeath' <$> eagerForDeath
  , EphemeralExhibits' <$> ephemeralExhibits
  , FalseLead' <$> falseLead
  , FinalRhapsody' <$> finalRhapsody
  , FrozenInFear' <$> frozenInFear
  , GraspingHands' <$> graspingHands
  , Haunted' <$> haunted
  , HospitalDebts' <$> hospitalDebts
  , HuntedDown' <$> huntedDown
  , HuntingShadow' <$> huntingShadow
  , Hypochondria' <$> hypochondria
  , Indebted' <$> indebted
  , InsatiableBloodlust' <$> insatiableBloodlust
  , InternalInjury' <$> internalInjury
  , Kidnapped' <$> kidnapped
  , LightOfAforgomon' <$> lightOfAforgomon
  , LockedDoor' <$> lockedDoor
  , LostInVenice' <$> lostInVenice
  , MaskOfUmordhoth' <$> maskOfUmordhoth
  , MaskedHorrors' <$> maskedHorrors
  , MassHysteria' <$> massHysteria
  , Mesmerize' <$> mesmerize
  , MysteriousChanting' <$> mysteriousChanting
  , ObscuringFog' <$> obscuringFog
  , OfferOfPower' <$> offerOfPower
  , OnTheProwl' <$> onTheProwl
  , OnWingsOfDarkness' <$> onWingsOfDarkness
  , Paranoia' <$> paranoia
  , PassageIntoTheVeil' <$> passageIntoTheVeil
  , PsychopompsSong' <$> psychopompsSong
  , Psychosis' <$> psychosis
  , PushedIntoTheBeyond' <$> pushedIntoTheBeyond
  , RexsCurse' <$> rexsCurse
  , RipplesOnTheSurface' <$> ripplesOnTheSurface
  , RitesHowled' <$> ritesHowled
  , RottingRemains' <$> rottingRemains
  , RottingRemainsBloodOnTheAltar' <$> rottingRemainsBloodOnTheAltar
  , RuinAndDestruction' <$> ruinAndDestruction
  , SearchingForIzzie' <$> searchingForIzzie
  , ShadowSpawned' <$> shadowSpawned
  , SlitheringBehindYou' <$> slitheringBehindYou
  , SmiteTheWicked' <$> smiteTheWicked
  , SomethingInTheDrinks' <$> somethingInTheDrinks
  , SordidAndSilent' <$> sordidAndSilent
  , SpacesBetween' <$> spacesBetween
  , SpectralMist' <$> spectralMist
  , StalkedInTheDark' <$> stalkedInTheDark
  , StrangeSigns' <$> strangeSigns
  , TerrorFromBeyond' <$> terrorFromBeyond
  , TheCreaturesTracks' <$> theCreaturesTracks
  , TheYellowSign' <$> theYellowSign
  , TheZealotsSeal' <$> theZealotsSeal
  , ToweringBeasts' <$> toweringBeasts
  , TwistOfFate' <$> twistOfFate
  , UmordhothsHunger' <$> umordhothsHunger
  , UmordhothsWrath' <$> umordhothsWrath
  , UnhallowedCountry' <$> unhallowedCountry
  , VastExpanse' <$> vastExpanse
  , VaultOfEarthlyDemise' <$> vaultOfEarthlyDemise
  , VisionsOfFuturesPast' <$> visionsOfFuturesPast
  , VortexOfTime' <$> vortexOfTime
  , WatchersGaze' <$> watchersGaze
  , Wormhole' <$> wormhole
  , WrackedByNightmares' <$> wrackedByNightmares
  ]

newtype BaseTreachery = BaseTreachery TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseTreachery :: CardCode -> InvestigatorId -> TreacheryId -> Treachery
baseTreachery cardCode iid tid = BaseTreachery' $ cbCardBuilder
  (treachery BaseTreachery (testCardDef TreacheryType cardCode))
  (iid, tid)

instance HasActions env BaseTreachery where
  getActions x window (BaseTreachery attrs) = getActions x window attrs

instance HasModifiersFor env BaseTreachery

instance (TreacheryRunner env) => RunMessage env BaseTreachery where
  runMessage msg (BaseTreachery attrs) = BaseTreachery <$> runMessage msg attrs

isWeakness :: Treachery -> Bool
isWeakness = cdWeakness . toCardDef

instance CanBeWeakness env Treachery where
  getIsWeakness = pure . isWeakness

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
