module Arkham.Types.Treachery
  ( module Arkham.Types.Treachery
  )
where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait (Trait)
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Cards
import Arkham.Types.Treachery.Runner
import Arkham.Types.TreacheryId

createTreachery :: IsCard a => a -> Maybe InvestigatorId -> Treachery
createTreachery a miid =
  lookupTreachery (getCardCode a) (TreacheryId $ getCardId a) miid

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
  | AcrossTimeAndSpace' AcrossTimeAndSpace
  | ClawsOfSteam' ClawsOfSteam
  | BrokenRails' BrokenRails
  | Kidnapped' Kidnapped
  | PsychopompsSong' PsychopompsSong
  | StrangeSigns' StrangeSigns
  | RottingRemainsBloodOnTheAltar' RottingRemainsBloodOnTheAltar
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Treachery
deriving anyclass instance TreacheryRunner env => RunMessage env Treachery
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
  getCardId = getCardId . toAttrs
  getCardCode = getCardCode . toAttrs
  getTraits = getTraits . toAttrs
  getKeywords = getKeywords . toAttrs

instance HasCount DoomCount env Treachery where
  getCount = pure . DoomCount . treacheryDoom . toAttrs

instance HasCount ResourceCount env Treachery where
  getCount = getCount . toAttrs

instance HasCount (Maybe ClueCount) env Treachery where
  getCount = pure . (ClueCount <$>) . treacheryClues . toAttrs

instance HasId (Maybe OwnerId) env Treachery where
  getId = pure . (OwnerId <$>) . treacheryOwner . toAttrs

lookupTreachery
  :: CardCode -> (TreacheryId -> Maybe InvestigatorId -> Treachery)
lookupTreachery cardCode =
  fromJustNote ("Unknown treachery: " <> pack (show cardCode))
    $ lookup cardCode allTreacheries

allTreacheries
  :: HashMap CardCode (TreacheryId -> Maybe InvestigatorId -> Treachery)
allTreacheries = mapFromList
  [ ("01007", (CoverUp' .) . coverUp)
  , ("01011", (HospitalDebts' .) . hospitalDebts)
  , ("01015", (AbandonedAndAlone' .) . abandonedAndAlone)
  , ("01096", (Amnesia' .) . amnesia)
  , ("01097", (Paranoia' .) . paranoia)
  , ("01098", (Haunted' .) . haunted)
  , ("01099", (Psychosis' .) . psychosis)
  , ("01100", (Hypochondria' .) . hypochondria)
  , ("01135", (HuntingShadow' .) . huntingShadow)
  , ("01136", (FalseLead' .) . falseLead)
  , ("01158", (UmordhothsWrath' .) . umordhothsWrath)
  , ("01162", (GraspingHands' .) . graspingHands)
  , ("01166", (AncientEvils' .) . ancientEvils)
  , ("01163", (RottingRemains' .) . rottingRemains)
  , ("01164", (FrozenInFear' .) . frozenInFear)
  , ("01165", (DissonantVoices' .) . dissonantVoices)
  , ("01167", (CryptChill' .) . cryptChill)
  , ("01168", (ObscuringFog' .) . obscuringFog)
  , ("01171", (MysteriousChanting' .) . mysteriousChanting)
  , ("01173", (OnWingsOfDarkness' .) . onWingsOfDarkness)
  , ("01174", (LockedDoor' .) . lockedDoor)
  , ("01176", (TheYellowSign' .) . theYellowSign)
  , ("01178", (OfferOfPower' .) . offerOfPower)
  , ("01182", (DreamsOfRlyeh' .) . dreamsOfRlyeh)
  , ("02007", (SmiteTheWicked' .) . smiteTheWicked)
  , ("02009", (RexsCurse' .) . rexsCurse)
  , ("02011", (SearchingForIzzie' .) . searchingForIzzie)
  , ("02013", (FinalRhapsody' .) . finalRhapsody)
  , ("02015", (WrackedByNightmares' .) . wrackedByNightmares)
  , ("02037", (Indebted' .) . indebted)
  , ("02038", (InternalInjury' .) . internalInjury)
  , ("02039", (Chronophobia' .) . chronophobia)
  , ("02081", (SomethingInTheDrinks' .) . somethingInTheDrinks)
  , ("02082", (ArousingSuspicions' .) . arousingSuspicions)
  , ("02083", (VisionsOfFuturesPast' .) . visionsOfFuturesPast)
  , ("02084", (BeyondTheVeil' .) . beyondTheVeil)
  , ("02085", (LightOfAforgomon' .) . lightOfAforgomon)
  , ("02088", (UnhallowedCountry' .) . unhallowedCountry)
  , ("02089", (SordidAndSilent' .) . sordidAndSilent)
  , ("02091", (EagerForDeath' .) . eagerForDeath)
  , ("02092", (CursedLuck' .) . cursedLuck)
  , ("02093", (TwistOfFate' .) . twistOfFate)
  , ("02096", (AlteredBeast' .) . alteredBeast)
  , ("02099", (HuntedDown' .) . huntedDown)
  , ("02100", (PushedIntoTheBeyond' .) . pushedIntoTheBeyond)
  , ("02101", (TerrorFromBeyond' .) . terrorFromBeyond)
  , ("02102", (ArcaneBarrier' .) . arcaneBarrier)
  , ("02142", (ShadowSpawned' .) . shadowSpawned)
  , ("02143", (StalkedInTheDark' .) . stalkedInTheDark)
  , ("02144", (PassageIntoTheVeil' .) . passageIntoTheVeil)
  , ("02145", (EphemeralExhibits' .) . ephemeralExhibits)
  , ("02146", (SlitheringBehindYou' .) . slitheringBehindYou)
  , ("02178", (AcrossTimeAndSpace' .) . acrossTimeAndSpace)
  , ("02180", (ClawsOfSteam' .) . clawsOfSteam)
  , ("02181", (BrokenRails' .) . brokenRails)
  , ("02220", (Kidnapped' .) . kidnapped)
  , ("02221", (PsychopompsSong' .) . psychopompsSong)
  , ("02222", (StrangeSigns' .) . strangeSigns)
  , ( "02223"
    , (RottingRemainsBloodOnTheAltar' .) . rottingRemainsBloodOnTheAltar
    )
  , ("50024", (TheZealotsSeal' .) . theZealotsSeal)
  , ("50031", (MaskedHorrors' .) . maskedHorrors)
  , ("50032b", (VaultOfEarthlyDemise' .) . vaultOfEarthlyDemise)
  , ("50037", (UmordhothsHunger' .) . umordhothsHunger)
  , ("50040", (ChillFromBelow' .) . chillFromBelow)
  , ("50043", (MaskOfUmordhoth' .) . maskOfUmordhoth)
  , ("60504", (Atychiphobia' .) . atychiphobia)
  , ("81024", (CursedSwamp' .) . cursedSwamp)
  , ("81025", (SpectralMist' .) . spectralMist)
  , ("81026", (DraggedUnder' .) . draggedUnder)
  , ("81027", (RipplesOnTheSurface' .) . ripplesOnTheSurface)
  , ("81029", (CurseOfTheRougarou' .) . curseOfTheRougarou)
  , ("81034", (OnTheProwl' .) . onTheProwl)
  , ("81035", (BeastOfTheBayou' .) . beastOfTheBayou)
  , ("81036", (InsatiableBloodlust' .) . insatiableBloodlust)
  ]

isWeakness :: Treachery -> Bool
isWeakness = treacheryWeakness . toAttrs

instance CanBeWeakness env Treachery where
  getIsWeakness = pure . isWeakness

treacheryTarget :: Treachery -> Maybe Target
treacheryTarget = treacheryAttachedTarget . toAttrs
