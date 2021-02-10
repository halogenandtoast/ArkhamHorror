module Arkham.Types.Asset
  ( module Arkham.Types.Asset
  , module X
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Class as X
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Trait (Trait)

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (getCardCode a) (AssetId $ getCardId a)

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
  | HelplessPassenger' HelplessPassenger
  | KeenEye3' KeenEye3
  | KeyToTheChamber' KeyToTheChamber
  | ZebulonWhateley' ZebulonWhateley
  | EarlSawyer' EarlSawyer
  | PowderOfIbnGhazi' PowderOfIbnGhazi
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ActionRunner env => HasActions env Asset where
  getActions iid window asset = do
    inPlay <- member (toId asset) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource asset) (toTarget asset) ()
      else pure []
    if any isBlank modifiers'
      then getActions iid window (toAttrs asset)
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
    inPlay <- member (toId asset) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource asset) (toTarget asset) ()
      else pure []
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' asset

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs

instance NamedEntity Asset where
  toName = toName . toAttrs

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Asset where
  toCard = toCard . toAttrs
  getCardId = getCardId . toAttrs
  getCardCode = getCardCode . toAttrs
  getTraits = getTraits . toAttrs
  getKeywords = getKeywords . toAttrs

newtype BaseAsset = BaseAsset AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseAsset :: AssetId -> CardCode -> (AssetAttrs -> AssetAttrs) -> Asset
baseAsset a b f = BaseAsset' . BaseAsset . f $ baseAttrs a b

instance HasDamage Asset where
  getDamage a =
    let AssetAttrs {..} = toAttrs a in (assetHealthDamage, assetSanityDamage)

instance HasActions env BaseAsset where
  getActions iid window (BaseAsset attrs) = getActions iid window attrs

instance HasModifiersFor env BaseAsset where
  getModifiersFor = noModifiersFor

instance AssetRunner env => RunMessage env BaseAsset where
  runMessage msg (BaseAsset attrs) = BaseAsset <$> runMessage msg attrs

instance Exhaustable Asset where
  isExhausted = assetExhausted . toAttrs

instance Discardable Asset where
  canBeDiscarded = assetCanLeavePlayByNormalMeans . toAttrs

instance HasId (Maybe OwnerId) env Asset where
  getId = pure . fmap OwnerId . assetInvestigator . toAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . assetLocation . toAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . toAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . toAttrs

instance HasCount UsesCount env Asset where
  getCount asset = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs asset)

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
  , ("02179", HelplessPassenger' . helplessPassenger)
  , ("02185", KeenEye3' . keenEye3)
  , ("02215", KeyToTheChamber' . keyToTheChamber)
  , ("02217", ZebulonWhateley' . zebulonWhateley)
  , ("02218", EarlSawyer' . earlSawyer)
  , ("02219", PowderOfIbnGhazi' . powderOfIbnGhazi)
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
  slotsOf = slotsOf . toAttrs
  useTypeOf = useTypeOf . toAttrs
  isHealthDamageable = isHealthDamageable . toAttrs
  isSanityDamageable = isSanityDamageable . toAttrs
  isStory = isStory . toAttrs
