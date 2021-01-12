module Arkham.Types.Asset
  ( lookupAsset
  , baseAsset
  , allAssets
  , isHealthDamageable
  , isSanityDamageable
  , isStory
  , slotsOf
  , useTypeOf
  , Asset
  ) where

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
  | Blackjack' Blackjack
  | LaboratoryAssistant' LaboratoryAssistant
  | StrangeSolution' StrangeSolution
  | LiquidCourage' LiquidCourage
  | HiredMuscle1' HiredMuscle1
  | RiteOfSeeking' RiteOfSeeking
  | RitualCandles' RitualCandles
  | ClarityOfMind' ClarityOfMind
  | FireAxe' FireAxe
  | PeterSylvestre' PeterSylvestre
  | PeterSylvestre2' PeterSylvestre2
  | Kukri' Kukri
  | DrHenryArmitage' DrHenryArmitage
  | AlchemicalConcoction' AlchemicalConcoction
  | JazzMulligan' JazzMulligan
  | ProfessorWarrenRice' ProfessorWarrenRice
  | PeterClover' PeterClover
  | DrFrancisMorgan' DrFrancisMorgan
  | BrotherXavier1' BrotherXavier1
  | Pathfinder1' Pathfinder1
  | Adaptable1' Adaptable1
  | HaroldWalsted' HaroldWalsted
  | AdamLynch' AdamLynch
  | TheNecronomiconOlausWormiusTranslation' TheNecronomiconOlausWormiusTranslation
  | Bandolier' Bandolier
  | KeenEye3' KeenEye3
  | SpringfieldM19034' SpringfieldM19034
  | LightningGun5' LightningGun5
  | ToothOfEztli' ToothOfEztli
  | OccultLexicon' OccultLexicon
  | ScrollOfProphecies' ScrollOfProphecies
  | KeenEye' KeenEye
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

instance ActionRunner env => HasActions env Asset where
  getActions iid window asset = do
    modifiers' <- getModifiersFor (toSource asset) (toTarget asset) ()
    if any isBlank modifiers'
      then getActions iid window (assetAttrs asset)
      else defaultGetActions iid window asset

deriving anyclass instance
  ( HasId LocationId env InvestigatorId
  , HasId CardCode env EnemyId
  , HasId (Maybe LocationId) env LocationMatcher
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount AssetCount env (InvestigatorId, [Trait])
  , HasSet Trait env LocationId
  , HasTarget ForSkillTest env
  , HasModifiersFor env ()
  )
  => HasModifiersFor env Asset

instance AssetRunner env => RunMessage env Asset where
  runMessage msg asset = do
    modifiers' <- getModifiersFor (toSource asset) (toTarget asset) ()
    if any isBlank modifiers'
      then runMessage (Blanked msg) asset
      else defaultRunMessage msg asset

instance Entity Asset where
  type EntityId Asset = AssetId
  toId = toId . assetAttrs
  toTarget = toTarget . assetAttrs
  isTarget = isTarget . assetAttrs
  toSource = toSource . assetAttrs
  isSource = isSource . assetAttrs

instance IsCard Asset where
  toCard = toCard . assetAttrs
  getCardId = getCardId . assetAttrs
  getCardCode = getCardCode . assetAttrs
  getTraits = getTraits . assetAttrs
  getKeywords = getKeywords . assetAttrs

newtype BaseAsset = BaseAsset Attrs
  deriving newtype (Show, ToJSON, FromJSON)

baseAsset :: AssetId -> CardCode -> (Attrs -> Attrs) -> Asset
baseAsset a b f = BaseAsset' . BaseAsset . f $ baseAttrs a b

instance HasDamage Asset where
  getDamage a =
    let Attrs {..} = assetAttrs a in (assetHealthDamage, assetSanityDamage)

instance HasActions env BaseAsset where
  getActions iid window (BaseAsset attrs) = getActions iid window attrs

instance HasModifiersFor env BaseAsset where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env BaseAsset where
  runMessage msg (BaseAsset attrs) = BaseAsset <$> runMessage msg attrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . assetAttrs

instance Discardable Asset where
  canBeDiscarded = assetCanLeavePlayByNormalMeans . assetAttrs

instance HasId (Maybe OwnerId) env Asset where
  getId = pure . fmap OwnerId . assetInvestigator . assetAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . assetLocation . assetAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . assetAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . assetAttrs

instance HasCount UsesCount env Asset where
  getCount asset = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (assetAttrs asset)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

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
  , ("02016", Blackjack' . blackjack)
  , ("02020", LaboratoryAssistant' . laboratoryAssistant)
  , ("02021", StrangeSolution' . strangeSolution)
  , ("02024", LiquidCourage' . liquidCourage)
  , ("02027", HiredMuscle1' . hiredMuscle1)
  , ("02028", RiteOfSeeking' . riteOfSeeking)
  , ("02029", RitualCandles' . ritualCandles)
  , ("02030", ClarityOfMind' . clarityOfMind)
  , ("02032", FireAxe' . fireAxe)
  , ("02033", PeterSylvestre' . peterSylvestre)
  , ("02035", PeterSylvestre2' . peterSylvestre2)
  , ("02036", Kukri' . kukri)
  , ("02040", DrHenryArmitage' . drHenryArmitage)
  , ("02059", AlchemicalConcoction' . alchemicalConcoction)
  , ("02060", JazzMulligan' . jazzMulligan)
  , ("02061", ProfessorWarrenRice' . professorWarrenRice)
  , ("02079", PeterClover' . peterClover)
  , ("02080", DrFrancisMorgan' . drFrancisMorgan)
  , ("02106", BrotherXavier1' . brotherXavier1)
  , ("02108", Pathfinder1' . pathfinder1)
  , ("02110", Adaptable1' . adaptable1)
  , ("02138", HaroldWalsted' . haroldWalsted)
  , ("02139", AdamLynch' . adamLynch)
  , ( "02140"
    , TheNecronomiconOlausWormiusTranslation'
      . theNecronomiconOlausWormiusTranslation
    )
  , ("02147", Bandolier' . bandolier)
  , ("02185", KeenEye3' . keenEye3)
  , ("02226", SpringfieldM19034' . springfieldM19034)
  , ("02301", LightningGun5' . lightningGun5)
  , ("04023", ToothOfEztli' . toothOfEztli)
  , ("05316", OccultLexicon' . occultLexicon)
  , ("06116", ScrollOfProphecies' . scrollOfProphecies)
  , ("07152", KeenEye' . keenEye)
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
  slotsOf = slotsOf . assetAttrs
  useTypeOf = useTypeOf . assetAttrs
  isHealthDamageable = isHealthDamageable . assetAttrs
  isSanityDamageable = isSanityDamageable . assetAttrs
  isStory = isStory . assetAttrs

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
  Blackjack' attrs -> coerce attrs
  LaboratoryAssistant' attrs -> coerce attrs
  StrangeSolution' attrs -> coerce attrs
  LiquidCourage' attrs -> coerce attrs
  HiredMuscle1' attrs -> coerce attrs
  RiteOfSeeking' attrs -> coerce attrs
  RitualCandles' attrs -> coerce attrs
  ClarityOfMind' attrs -> coerce attrs
  FireAxe' attrs -> coerce attrs
  PeterSylvestre' attrs -> coerce attrs
  PeterSylvestre2' attrs -> coerce attrs
  Kukri' attrs -> coerce attrs
  DrHenryArmitage' attrs -> coerce attrs
  AlchemicalConcoction' attrs -> coerce attrs
  JazzMulligan' attrs -> coerce attrs
  ProfessorWarrenRice' attrs -> coerce attrs
  PeterClover' attrs -> coerce attrs
  DrFrancisMorgan' attrs -> coerce attrs
  BrotherXavier1' attrs -> coerce attrs
  Pathfinder1' attrs -> coerce attrs
  Adaptable1' attrs -> coerce attrs
  HaroldWalsted' attrs -> coerce attrs
  AdamLynch' attrs -> coerce attrs
  TheNecronomiconOlausWormiusTranslation' attrs -> coerce attrs
  Bandolier' attrs -> coerce attrs
  KeenEye3' attrs -> coerce attrs
  SpringfieldM19034' attrs -> coerce attrs
  LightningGun5' attrs -> coerce attrs
  ToothOfEztli' attrs -> coerce attrs
  OccultLexicon' attrs -> coerce attrs
  ScrollOfProphecies' attrs -> coerce attrs
  KeenEye' attrs -> coerce attrs
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
  HigherEducation' attrs -> coerce attrs
  WhittonGreene' attrs -> coerce attrs
  LadyEsprit' attrs -> coerce attrs
  BearTrap' attrs -> coerce attrs
  FishingNet' attrs -> coerce attrs
  MonstrousTransformation' attrs -> coerce attrs
  DaisysToteBagAdvanced' attrs -> coerce attrs
  TheNecronomiconAdvanced' attrs -> coerce attrs
  BaseAsset' attrs -> coerce attrs
