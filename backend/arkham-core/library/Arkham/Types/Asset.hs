{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset
  ( lookupAsset
  , baseAsset
  , allAssets
  , IsAsset(..)
  , Asset
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Class
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Trait (Trait)

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
  | ToothOfEztli' ToothOfEztli
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
  | HigherEducation' HigherEducation
  | WhittonGreene' WhittonGreene
  | LadyEsprit' LadyEsprit
  | BearTrap' BearTrap
  | FishingNet' FishingNet
  | MonstrousTransformation' MonstrousTransformation
  | DaisysToteBagAdvanced' DaisysToteBagAdvanced
  | TheNecronomiconAdvanced' TheNecronomiconAdvanced
  | BaseAsset' BaseAsset
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance ActionRunner env => HasActions env Asset
deriving anyclass instance
  ( HasId LocationId env InvestigatorId
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount AssetCount env (InvestigatorId, [Trait])
  )
  => HasModifiersFor env Asset
deriving anyclass instance AssetRunner env => RunMessage env Asset

instance HasAttrs Asset where
  type AttrsT Asset = Attrs
  toAttrs = toAttrs . toAttrs

instance Entity Asset where
  type EntityId Asset = AssetId
  toId = toId . toAttrs
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Asset where
  toCard a = PlayerCard $ lookupPlayerCard (getCardCode a) (getCardId a)

instance HasCardId Asset where
  getCardId = CardId . unAssetId . assetId . toAttrs

newtype BaseAsset = BaseAsset Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAsset :: AssetId -> CardCode -> (Attrs -> Attrs) -> Asset
baseAsset a b f = BaseAsset' . BaseAsset . f $ baseAttrs a b

instance HasDamage Asset where
  getDamage a =
    let Attrs {..} = toAttrs a in (assetHealthDamage, assetSanityDamage)

instance HasActions env BaseAsset where
  getActions iid window (BaseAsset attrs) = getActions iid window attrs

instance HasModifiersFor env BaseAsset where
  getModifiersFor = noModifiersFor

instance (AssetRunner env) => RunMessage env BaseAsset where
  runMessage msg (BaseAsset attrs) = BaseAsset <$> runMessage msg attrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . toAttrs

instance Discardable Asset where
  canBeDiscarded = assetCanLeavePlayByNormalMeans . toAttrs

instance HasCardCode Asset where
  getCardCode = assetCardCode . toAttrs

instance HasTraits Asset where
  getTraits = assetTraits . toAttrs

instance HasId AssetId env Asset where
  getId = pure . assetId . toAttrs

instance HasId (Maybe OwnerId) env Asset where
  getId = pure . (OwnerId <$>) . assetInvestigator . toAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . assetLocation . toAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . toAttrs

instance HasCount UsesCount env Asset where
  getCount asset = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs asset)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset = fromJustNote "Unknown asset" . flip lookup allAssets

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
  , ("04023", ToothOfEztli' . toothOfEztli)
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
  , ("60211", HigherEducation' . higherEducation)
  , ("60213", WhittonGreene' . whittonGreene)
  , ("81019", LadyEsprit' . ladyEsprit)
  , ("81020", BearTrap' . bearTrap)
  , ("81021", FishingNet' . fishingNet)
  , ("81030", MonstrousTransformation' . monstrousTransformation)
  , ("90002", DaisysToteBagAdvanced' . daisysToteBagAdvanced)
  , ("90003", TheNecronomiconAdvanced' . theNecronomiconAdvanced)
  , ("00000", \aid -> baseAsset aid "00000" id)
  ]

instance IsAsset Asset where
  slotsOf = slotsOf . toAttrs
  useTypeOf = useTypeOf . toAttrs
  isHealthDamageable = isHealthDamageable . toAttrs
  isSanityDamageable = isSanityDamageable . toAttrs
