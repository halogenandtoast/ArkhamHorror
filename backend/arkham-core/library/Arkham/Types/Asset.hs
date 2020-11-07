{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset
  ( lookupAsset
  , baseAsset
  , allAssets
  , isHealthDamageable
  , isSanityDamageable
  , slotsOf
  , Asset
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Data.Coerce

data Asset
  = Rolands38Special' Rolands38Special
  | DaisysToteBag' DaisysToteBag
  | TheNecronomicon' TheNecronomicon
  | HeirloomOfHyperborea' HeirloomOfHyperborea
  | WendysAmulet' WendysAmulet
  | FortyFiveAutomatic' FortyFiveAutomatic
  | PhysicalTraining' PhysicalTraining
  | BeatCop' BeatCop
  | FirstAid' FirstAid
  | Machete' Machete
  | GuardDog' GuardDog
  | PoliceBadge2' PoliceBadge2
  | BeatCop2' BeatCop2
  | Shotgun4' Shotgun4
  | MagnifyingGlass' MagnifyingGlass
  | OldBookOfLore' OldBookOfLore
  | ResearchLibrarian' ResearchLibrarian
  | DrMilanChristopher' DrMilanChristopher
  | Hyperawareness' Hyperawareness
  | MedicalTexts' MedicalTexts
  | MagnifyingGlass1' MagnifyingGlass1
  | DiscOfItzamna2' DiscOfItzamna2
  | Encyclopedia2' Encyclopedia2
  | Switchblade' Switchblade
  | Burglary' Burglary
  | Pickpocketing' Pickpocketing
  | FortyOneDerringer' FortyOneDerringer
  | LeoDeLuca' LeoDeLuca
  | HardKnocks' HardKnocks
  | LeoDeLuca1' LeoDeLuca1
  | CatBurgler1' CatBurgler1
  | ForbiddenKnowledge' ForbiddenKnowledge
  | HolyRosary' HolyRosary
  | Shrivelling' Shrivelling
  | Scrying' Scrying
  | ArcaneStudies' ArcaneStudies
  | ArcaneInitiate' ArcaneInitiate
  | BookOfShadows3' BookOfShadows3
  | GrotesqueStatue4' GrotesqueStatue4
  | LeatherCoat' LeatherCoat
  | Scavenging' Scavenging
  | BaseballBat' BaseballBat
  | RabbitsFoot' RabbitsFoot
  | StrayCat' StrayCat
  | DigDeep' DigDeep
  | Aquinnah1' Aquinnah1
  | Knife' Knife
  | Flashlight' Flashlight
  | BulletproofVest3' BulletproofVest3
  | ElderSignAmulet3' ElderSignAmulet3
  | LitaChantler' LitaChantler
  | ZoeysCross' ZoeysCross
  | JennysTwin45s' JennysTwin45s
  | JimsTrumpet' JimsTrumpet
  | Duke' Duke
  | FireAxe' FireAxe
  | PeterSylvestre' PeterSylvestre
  | Bandolier' Bandolier
  | OccultLexicon' OccultLexicon
  | ScrollOfProphecies' ScrollOfProphecies
  | PhysicalTraining2' PhysicalTraining2
  | Hyperawareness2' Hyperawareness2
  | HardKnocks2' HardKnocks2
  | ArcaneStudies2' ArcaneStudies2
  | DigDeep2' DigDeep2
  | RabbitsFoot3' RabbitsFoot3
  | ArcaneEnlightenment' ArcaneEnlightenment
  | CelaenoFragments' CelaenoFragments
  | Encyclopedia' Encyclopedia
  | LadyEsprit' LadyEsprit
  | BearTrap' BearTrap
  | FishingNet' FishingNet
  | MonstrousTransformation' MonstrousTransformation
  | TheNecronomiconAdvanced' TheNecronomiconAdvanced
  | BaseAsset' BaseAsset
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Asset
deriving anyclass instance AssetRunner env => HasModifiersFor env Asset
deriving anyclass instance AssetRunner env => RunMessage env Asset

instance IsCard Asset where
  toCard a = PlayerCard $ lookupPlayerCard (getCardCode a) (getCardId a)

instance HasCardId Asset where
  getCardId = CardId . unAssetId . assetId . assetAttrs

newtype BaseAsset = BaseAsset Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAsset :: AssetId -> CardCode -> (Attrs -> Attrs) -> Asset
baseAsset a b f = BaseAsset' . BaseAsset . f $ baseAttrs a b (pure ())

instance HasDamage Asset where
  getDamage a =
    let Attrs {..} = assetAttrs a in (assetHealthDamage, assetSanityDamage)

instance HasActions env BaseAsset where
  getActions iid window (BaseAsset attrs) = getActions iid window attrs

instance HasModifiersFor env BaseAsset where
  getModifiersFor _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BaseAsset where
  runMessage msg (BaseAsset attrs) = BaseAsset <$> runMessage msg attrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . assetAttrs

instance Discardable Asset where
  canBeDiscarded = assetCanLeavePlayByNormalMeans . assetAttrs

instance HasCardCode Asset where
  getCardCode = assetCardCode . assetAttrs

instance HasTraits Asset where
  getTraits = assetTraits . assetAttrs

instance HasId AssetId () Asset where
  getId _ = assetId . assetAttrs

instance HasId (Maybe OwnerId) () Asset where
  getId _ = (OwnerId <$>) . assetInvestigator . assetAttrs

instance HasId (Maybe LocationId) () Asset where
  getId _ = assetLocation . assetAttrs

instance HasCount DoomCount () Asset where
  getCount _ = DoomCount . assetDoom . assetAttrs

instance HasCount UsesCount () Asset where
  getCount _ asset = case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (assetAttrs asset)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset = fromJustNote "Unkown asset" . flip lookup allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = mapFromList
  [ ("01006", Rolands38Special' . rolands38Special)
  , ("01008", DaisysToteBag' . daisysToteBag)
  , ("01009", TheNecronomicon' . theNecronomicon)
  , ("01012", HeirloomOfHyperborea' . heirloomOfHyperborea)
  , ("01014", WendysAmulet' . wendysAmulet)
  , ("01016", FortyFiveAutomatic' . fortyFiveAutomatic)
  , ("01017", PhysicalTraining' . physicalTraining)
  , ("01018", BeatCop' . beatCop)
  , ("01019", FirstAid' . firstAid)
  , ("01020", Machete' . machete)
  , ("01021", GuardDog' . guardDog)
  , ("01027", PoliceBadge2' . policeBadge2)
  , ("01028", BeatCop2' . beatCop2)
  , ("01029", Shotgun4' . shotgun4)
  , ("01030", MagnifyingGlass' . magnifyingGlass)
  , ("01031", OldBookOfLore' . oldBookOfLore)
  , ("01032", ResearchLibrarian' . researchLibrarian)
  , ("01033", DrMilanChristopher' . drMilanChristopher)
  , ("01034", Hyperawareness' . hyperawareness)
  , ("01035", MedicalTexts' . medicalTexts)
  , ("01040", MagnifyingGlass1' . magnifyingGlass1)
  , ("01041", DiscOfItzamna2' . discOfItzamna2)
  , ("01042", Encyclopedia2' . encyclopedia2)
  , ("01044", Switchblade' . switchblade)
  , ("01045", Burglary' . burglary)
  , ("01046", Pickpocketing' . pickpoketing)
  , ("01047", FortyOneDerringer' . fortyOneDerringer)
  , ("01048", LeoDeLuca' . leoDeLuca)
  , ("01049", HardKnocks' . hardKnocks)
  , ("01054", LeoDeLuca1' . leoDeLuca1)
  , ("01055", CatBurgler1' . catBurgler1)
  , ("01058", ForbiddenKnowledge' . forbiddenKnowledge)
  , ("01059", HolyRosary' . holyRosary)
  , ("01060", Shrivelling' . shrivelling)
  , ("01061", Scrying' . scrying)
  , ("01062", ArcaneStudies' . arcaneStudies)
  , ("01063", ArcaneInitiate' . arcaneInitiate)
  , ("01070", BookOfShadows3' . bookOfShadows3)
  , ("01071", GrotesqueStatue4' . grotesqueStatue4)
  , ("01072", LeatherCoat' . leatherCoat)
  , ("01073", Scavenging' . scavenging)
  , ("01074", BaseballBat' . baseballBat)
  , ("01075", RabbitsFoot' . rabbitsFoot)
  , ("01076", StrayCat' . strayCat)
  , ("01077", DigDeep' . digDeep)
  , ("01082", Aquinnah1' . aquinnah1)
  , ("01086", Knife' . knife)
  , ("01087", Flashlight' . flashlight)
  , ("01094", BulletproofVest3' . bulletproofVest3)
  , ("01095", ElderSignAmulet3' . elderSignAmulet3)
  , ("01117", LitaChantler' . litaChantler)
  , ("02006", ZoeysCross' . zoeysCross)
  , ("02010", JennysTwin45s' . jennysTwin45s)
  , ("02012", JimsTrumpet' . jimsTrumpet)
  , ("02014", Duke' . duke)
  , ("02032", FireAxe' . fireAxe)
  , ("02033", PeterSylvestre' . peterSylvestre)
  , ("02147", Bandolier' . bandolier)
  , ("05316", OccultLexicon' . occultLexicon)
  , ("06116", ScrollOfProphecies' . scrollOfProphecies)
  , ("50001", PhysicalTraining2' . physicalTraining2)
  , ("50003", Hyperawareness2' . hyperawareness2)
  , ("50005", HardKnocks2' . hardKnocks2)
  , ("50007", ArcaneStudies2' . arcaneStudies2)
  , ("50009", DigDeep2' . digDeep2)
  , ("50010", RabbitsFoot3' . rabbitsFoot3)
  , ("60205", ArcaneEnlightenment' . arcaneEnlightenment)
  , ("60206", CelaenoFragments' . celaenoFragments)
  , ("60208", Encyclopedia' . encyclopedia)
  , ("81019", LadyEsprit' . ladyEsprit)
  , ("81020", BearTrap' . bearTrap)
  , ("81021", FishingNet' . fishingNet)
  , ("81030", MonstrousTransformation' . monstrousTransformation)
  , ("90003", TheNecronomiconAdvanced' . theNecronomiconAdvanced)
  , ("00000", \aid -> baseAsset aid "00000" id)
  ]

slotsOf :: Asset -> [SlotType]
slotsOf = assetSlots . assetAttrs

isHealthDamageable :: Asset -> Bool
isHealthDamageable a = case assetHealth (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetHealthDamage (assetAttrs a)

isSanityDamageable :: Asset -> Bool
isSanityDamageable a = case assetSanity (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetSanityDamage (assetAttrs a)

assetAttrs :: Asset -> Attrs
assetAttrs = \case
  Rolands38Special' attrs -> coerce attrs
  DaisysToteBag' attrs -> coerce attrs
  TheNecronomicon' attrs -> coerce attrs
  HeirloomOfHyperborea' attrs -> coerce attrs
  WendysAmulet' attrs -> coerce attrs
  FortyFiveAutomatic' attrs -> coerce attrs
  PhysicalTraining' attrs -> coerce attrs
  BeatCop' attrs -> coerce attrs
  FirstAid' attrs -> coerce attrs
  Machete' attrs -> coerce attrs
  GuardDog' attrs -> coerce attrs
  PoliceBadge2' attrs -> coerce attrs
  BeatCop2' attrs -> coerce attrs
  Shotgun4' attrs -> coerce attrs
  MagnifyingGlass' attrs -> coerce attrs
  OldBookOfLore' attrs -> coerce attrs
  ResearchLibrarian' attrs -> coerce attrs
  DrMilanChristopher' attrs -> coerce attrs
  Hyperawareness' attrs -> coerce attrs
  MedicalTexts' attrs -> coerce attrs
  MagnifyingGlass1' attrs -> coerce attrs
  DiscOfItzamna2' attrs -> coerce attrs
  Encyclopedia2' attrs -> coerce attrs
  Switchblade' attrs -> coerce attrs
  Burglary' attrs -> coerce attrs
  Pickpocketing' attrs -> coerce attrs
  FortyOneDerringer' attrs -> coerce attrs
  LeoDeLuca' attrs -> coerce attrs
  HardKnocks' attrs -> coerce attrs
  LeoDeLuca1' attrs -> coerce attrs
  CatBurgler1' attrs -> coerce attrs
  ForbiddenKnowledge' attrs -> coerce attrs
  HolyRosary' attrs -> coerce attrs
  Shrivelling' attrs -> coerce attrs
  Scrying' attrs -> coerce attrs
  ArcaneStudies' attrs -> coerce attrs
  ArcaneInitiate' attrs -> coerce attrs
  BookOfShadows3' attrs -> coerce attrs
  LeatherCoat' attrs -> coerce attrs
  Scavenging' attrs -> coerce attrs
  BaseballBat' attrs -> coerce attrs
  RabbitsFoot' attrs -> coerce attrs
  StrayCat' attrs -> coerce attrs
  DigDeep' attrs -> coerce attrs
  Aquinnah1' attrs -> coerce attrs
  Knife' attrs -> coerce attrs
  Flashlight' attrs -> coerce attrs
  BulletproofVest3' attrs -> coerce attrs
  ElderSignAmulet3' attrs -> coerce attrs
  LitaChantler' attrs -> coerce attrs
  ZoeysCross' attrs -> coerce attrs
  JennysTwin45s' attrs -> coerce attrs
  JimsTrumpet' attrs -> coerce attrs
  Duke' attrs -> coerce attrs
  FireAxe' attrs -> coerce attrs
  PeterSylvestre' attrs -> coerce attrs
  Bandolier' attrs -> coerce attrs
  OccultLexicon' attrs -> coerce attrs
  ScrollOfProphecies' attrs -> coerce attrs
  PhysicalTraining2' attrs -> coerce attrs
  Hyperawareness2' attrs -> coerce attrs
  HardKnocks2' attrs -> coerce attrs
  ArcaneStudies2' attrs -> coerce attrs
  GrotesqueStatue4' attrs -> coerce attrs
  DigDeep2' attrs -> coerce attrs
  RabbitsFoot3' attrs -> coerce attrs
  ArcaneEnlightenment' attrs -> coerce attrs
  CelaenoFragments' attrs -> coerce attrs
  Encyclopedia' attrs -> coerce attrs
  LadyEsprit' attrs -> coerce attrs
  BearTrap' attrs -> coerce attrs
  FishingNet' attrs -> coerce attrs
  MonstrousTransformation' attrs -> coerce attrs
  TheNecronomiconAdvanced' attrs -> coerce attrs
  BaseAsset' attrs -> coerce attrs
