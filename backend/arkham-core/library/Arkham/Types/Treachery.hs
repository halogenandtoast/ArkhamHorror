module Arkham.Types.Treachery
  ( module Arkham.Types.Treachery
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
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
  = CoverUp' CoverUp
  | HospitalDebts' HospitalDebts
  | AbandonedAndAlone' AbandonedAndAlone
  | Amnesia' Amnesia
  | Paranoia' Paranoia
  | Haunted' Haunted
  | Psychosis' Psychosis
  | Hypochondria' Hypochondria
  | HuntingShadow' HuntingShadow
  | FalseLead' FalseLead
  | UmordhothsWrath' UmordhothsWrath
  | GraspingHands' GraspingHands
  | AncientEvils' AncientEvils
  | RottingRemains' RottingRemains
  | FrozenInFear' FrozenInFear
  | DissonantVoices' DissonantVoices
  | CryptChill' CryptChill
  | ObscuringFog' ObscuringFog
  | MysteriousChanting' MysteriousChanting
  | OnWingsOfDarkness' OnWingsOfDarkness
  | LockedDoor' LockedDoor
  | TheYellowSign' TheYellowSign
  | OfferOfPower' OfferOfPower
  | DreamsOfRlyeh' DreamsOfRlyeh
  | SmiteTheWicked' SmiteTheWicked
  | RexsCurse' RexsCurse
  | SearchingForIzzie' SearchingForIzzie
  | FinalRhapsody' FinalRhapsody
  | WrackedByNightmares' WrackedByNightmares
  | Indebted' Indebted
  | InternalInjury' InternalInjury
  | Chronophobia' Chronophobia
  | SomethingInTheDrinks' SomethingInTheDrinks
  | ArousingSuspicions' ArousingSuspicions
  | VisionsOfFuturesPast' VisionsOfFuturesPast
  | BeyondTheVeil' BeyondTheVeil
  | LightOfAforgomon' LightOfAforgomon
  | UnhallowedCountry' UnhallowedCountry
  | SordidAndSilent' SordidAndSilent
  | EagerForDeath' EagerForDeath
  | CursedLuck' CursedLuck
  | TwistOfFate' TwistOfFate
  | AlteredBeast' AlteredBeast
  | HuntedDown' HuntedDown
  | PushedIntoTheBeyond' PushedIntoTheBeyond
  | TerrorFromBeyond' TerrorFromBeyond
  | ArcaneBarrier' ArcaneBarrier
  | ShadowSpawned' ShadowSpawned
  | StalkedInTheDark' StalkedInTheDark
  | PassageIntoTheVeil' PassageIntoTheVeil
  | EphemeralExhibits' EphemeralExhibits
  | SlitheringBehindYou' SlitheringBehindYou
  | AcrossSpaceAndTime' AcrossSpaceAndTime
  | ClawsOfSteam' ClawsOfSteam
  | BrokenRails' BrokenRails
  | Kidnapped' Kidnapped
  | PsychopompsSong' PsychopompsSong
  | StrangeSigns' StrangeSigns
  | RottingRemainsBloodOnTheAltar' RottingRemainsBloodOnTheAltar
  | ToweringBeasts' ToweringBeasts
  | RuinAndDestruction' RuinAndDestruction
  | AttractingAttention' AttractingAttention
  | TheCreaturesTracks' TheCreaturesTracks
  | RitesHowled' RitesHowled
  | SpacesBetween' SpacesBetween
  | VortexOfTime' VortexOfTime
  | CollapsingReality' CollapsingReality
  | Wormhole' Wormhole
  | VastExpanse' VastExpanse
  | TheZealotsSeal' TheZealotsSeal
  | MaskedHorrors' MaskedHorrors
  | VaultOfEarthlyDemise' VaultOfEarthlyDemise
  | UmordhothsHunger' UmordhothsHunger
  | ChillFromBelow' ChillFromBelow
  | MaskOfUmordhoth' MaskOfUmordhoth
  | Atychiphobia' Atychiphobia
  | CursedSwamp' CursedSwamp
  | SpectralMist' SpectralMist
  | DraggedUnder' DraggedUnder
  | RipplesOnTheSurface' RipplesOnTheSurface
  | CurseOfTheRougarou' CurseOfTheRougarou
  | OnTheProwl' OnTheProwl
  | BeastOfTheBayou' BeastOfTheBayou
  | InsatiableBloodlust' InsatiableBloodlust
  | BaseTreachery' BaseTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Treachery where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Treachery
deriving anyclass instance (HasId CardCode env LocationId, TreacheryRunner env) => RunMessage env Treachery
deriving anyclass instance
  ( HasCount PlayerCount env ()
  , HasId LocationId env InvestigatorId
  , HasId (Maybe OwnerId) env AssetId
  , HasSet Trait env AssetId
  , HasSet Trait env LocationId
  , HasSet UniqueEnemyId env ()
  , HasCount ResourceCount env TreacheryId
  )
  => HasModifiersFor env Treachery

instance Entity Treachery where
  type EntityId Treachery = TreacheryId
  type EntityAttrs Treachery = TreacheryAttrs

instance NamedEntity Treachery where
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
  [ CoverUp' <$> coverUp
  , HospitalDebts' <$> hospitalDebts
  , AbandonedAndAlone' <$> abandonedAndAlone
  , Amnesia' <$> amnesia
  , Paranoia' <$> paranoia
  , Haunted' <$> haunted
  , Psychosis' <$> psychosis
  , Hypochondria' <$> hypochondria
  , HuntingShadow' <$> huntingShadow
  , FalseLead' <$> falseLead
  , UmordhothsWrath' <$> umordhothsWrath
  , GraspingHands' <$> graspingHands
  , AncientEvils' <$> ancientEvils
  , RottingRemains' <$> rottingRemains
  , FrozenInFear' <$> frozenInFear
  , DissonantVoices' <$> dissonantVoices
  , CryptChill' <$> cryptChill
  , ObscuringFog' <$> obscuringFog
  , MysteriousChanting' <$> mysteriousChanting
  , OnWingsOfDarkness' <$> onWingsOfDarkness
  , LockedDoor' <$> lockedDoor
  , TheYellowSign' <$> theYellowSign
  , OfferOfPower' <$> offerOfPower
  , DreamsOfRlyeh' <$> dreamsOfRlyeh
  , SmiteTheWicked' <$> smiteTheWicked
  , RexsCurse' <$> rexsCurse
  , SearchingForIzzie' <$> searchingForIzzie
  , FinalRhapsody' <$> finalRhapsody
  , WrackedByNightmares' <$> wrackedByNightmares
  , Indebted' <$> indebted
  , InternalInjury' <$> internalInjury
  , Chronophobia' <$> chronophobia
  , SomethingInTheDrinks' <$> somethingInTheDrinks
  , ArousingSuspicions' <$> arousingSuspicions
  , VisionsOfFuturesPast' <$> visionsOfFuturesPast
  , BeyondTheVeil' <$> beyondTheVeil
  , LightOfAforgomon' <$> lightOfAforgomon
  , UnhallowedCountry' <$> unhallowedCountry
  , SordidAndSilent' <$> sordidAndSilent
  , EagerForDeath' <$> eagerForDeath
  , CursedLuck' <$> cursedLuck
  , TwistOfFate' <$> twistOfFate
  , AlteredBeast' <$> alteredBeast
  , HuntedDown' <$> huntedDown
  , PushedIntoTheBeyond' <$> pushedIntoTheBeyond
  , TerrorFromBeyond' <$> terrorFromBeyond
  , ArcaneBarrier' <$> arcaneBarrier
  , ShadowSpawned' <$> shadowSpawned
  , StalkedInTheDark' <$> stalkedInTheDark
  , PassageIntoTheVeil' <$> passageIntoTheVeil
  , EphemeralExhibits' <$> ephemeralExhibits
  , SlitheringBehindYou' <$> slitheringBehindYou
  , AcrossSpaceAndTime' <$> acrossSpaceAndTime
  , ClawsOfSteam' <$> clawsOfSteam
  , BrokenRails' <$> brokenRails
  , Kidnapped' <$> kidnapped
  , PsychopompsSong' <$> psychopompsSong
  , StrangeSigns' <$> strangeSigns
  , RottingRemainsBloodOnTheAltar' <$> rottingRemainsBloodOnTheAltar
  , ToweringBeasts' <$> toweringBeasts
  , RuinAndDestruction' <$> ruinAndDestruction
  , AttractingAttention' <$> attractingAttention
  , TheCreaturesTracks' <$> theCreaturesTracks
  , RitesHowled' <$> ritesHowled
  , SpacesBetween' <$> spacesBetween
  , VortexOfTime' <$> vortexOfTime
  , CollapsingReality' <$> collapsingReality
  , Wormhole' <$> wormhole
  , VastExpanse' <$> vastExpanse
  , TheZealotsSeal' <$> theZealotsSeal
  , MaskedHorrors' <$> maskedHorrors
  , VaultOfEarthlyDemise' <$> vaultOfEarthlyDemise
  , UmordhothsHunger' <$> umordhothsHunger
  , ChillFromBelow' <$> chillFromBelow
  , MaskOfUmordhoth' <$> maskOfUmordhoth
  , Atychiphobia' <$> atychiphobia
  , CursedSwamp' <$> cursedSwamp
  , SpectralMist' <$> spectralMist
  , DraggedUnder' <$> draggedUnder
  , RipplesOnTheSurface' <$> ripplesOnTheSurface
  , CurseOfTheRougarou' <$> curseOfTheRougarou
  , OnTheProwl' <$> onTheProwl
  , BeastOfTheBayou' <$> beastOfTheBayou
  , InsatiableBloodlust' <$> insatiableBloodlust
  , CardBuilder
    { cbCardCode = "treachery"
    , cbCardBuilder = uncurry (baseTreachery "treachery")
    }
  ]

newtype BaseTreachery = BaseTreachery TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseTreachery :: CardCode -> InvestigatorId -> TreacheryId -> Treachery
baseTreachery cardCode iid tid = BaseTreachery' $ cbCardBuilder
  (treachery BaseTreachery (testCardDef TreacheryType cardCode))
  (iid, tid)

instance HasActions env BaseTreachery where
  getActions x window (BaseTreachery attrs) = getActions x window attrs

instance HasModifiersFor env BaseTreachery where
  getModifiersFor = noModifiersFor

instance (TreacheryRunner env) => RunMessage env BaseTreachery where
  runMessage msg (BaseTreachery attrs) = BaseTreachery <$> runMessage msg attrs

isWeakness :: Treachery -> Bool
isWeakness = cdWeakness . toCardDef

instance CanBeWeakness env Treachery where
  getIsWeakness = pure . isWeakness

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
