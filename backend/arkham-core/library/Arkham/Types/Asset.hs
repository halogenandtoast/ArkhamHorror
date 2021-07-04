module Arkham.Types.Asset
  ( module Arkham.Types.Asset
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards
import Arkham.Types.Asset.Class as X
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.Trait (Trait)

createAsset :: IsCard a => a -> Asset
createAsset a = lookupAsset (toCardCode a) (AssetId $ toCardId a)

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
  | CatBurglar1' CatBurglar1
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
  | SongOfTheDead2' SongOfTheDead2
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
  | EsotericFormula' EsotericFormula
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

instance (ActionRunner env, HasSkillTest env) => HasActions env Asset where
  getActions iid window x = do
    inPlay <- member (toId x) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource x) (toTarget x) ()
      else pure []
    if any isBlank modifiers'
      then getActions iid window (toAttrs x)
      else defaultGetActions iid window x

deriving anyclass instance
  ( HasId LocationId env InvestigatorId
  , HasId CardCode env EnemyId
  , HasId (Maybe LocationId) env LocationMatcher
  , HasCount ResourceCount env InvestigatorId
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env EnemyId
  , HasCount AssetCount env (InvestigatorId, [Trait])
  , HasSet Trait env LocationId
  , HasSkillTest env
  , HasModifiersFor env ()
  )
  => HasModifiersFor env Asset

instance AssetRunner env => RunMessage env Asset where
  runMessage msg x = do
    inPlay <- member (toId x) <$> getSet ()
    modifiers' <- if inPlay
      then getModifiersFor (toSource x) (toTarget x) ()
      else pure []
    let msg' = if any isBlank modifiers' then Blanked msg else msg
    defaultRunMessage msg' x

instance Entity Asset where
  type EntityId Asset = AssetId
  type EntityAttrs Asset = AssetAttrs

instance NamedEntity Asset where
  toName = toName . toAttrs

instance HasName env Asset where
  getName = pure . toName

instance TargetEntity Asset where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Asset where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardDef Asset where
  toCardDef = toCardDef . toAttrs

instance IsCard Asset where
  toCardId = toCardId . toAttrs

newtype BaseAsset = BaseAsset AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseAsset
  :: AssetId
  -> CardCode
  -> (AssetAttrs -> AssetAttrs)
  -> (CardDef -> CardDef)
  -> Asset
baseAsset aid cardCode attrsF defF = BaseAsset' $ cbCardBuilder
  (assetWith BaseAsset (defF $ testCardDef AssetType cardCode) attrsF)
  aid

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
  getId = pure . coerce . assetInvestigator . toAttrs

instance HasId (Maybe LocationId) env Asset where
  getId = pure . assetLocation . toAttrs

instance HasCount DoomCount env Asset where
  getCount = pure . DoomCount . assetDoom . toAttrs

instance HasCount ClueCount env Asset where
  getCount = pure . ClueCount . assetClues . toAttrs

instance HasCount UsesCount env Asset where
  getCount x = pure $ case uses' of
    NoUses -> UsesCount 0
    Uses _ n -> UsesCount n
    where uses' = assetUses (toAttrs x)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset cardCode =
  fromJustNote ("Unknown asset: " <> show cardCode) $ lookup cardCode allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = mapFromList $ map
  (cbCardCode &&& cbCardBuilder)
  [ Rolands38Special' <$> rolands38Special
  , DaisysToteBag' <$> daisysToteBag
  , TheNecronomicon' <$> theNecronomicon
  , HeirloomOfHyperborea' <$> heirloomOfHyperborea
  , WendysAmulet' <$> wendysAmulet
  , FortyFiveAutomatic' <$> fortyFiveAutomatic
  , PhysicalTraining' <$> physicalTraining
  , BeatCop' <$> beatCop
  , FirstAid' <$> firstAid
  , Machete' <$> machete
  , GuardDog' <$> guardDog
  , PoliceBadge2' <$> policeBadge2
  , BeatCop2' <$> beatCop2
  , Shotgun4' <$> shotgun4
  , MagnifyingGlass' <$> magnifyingGlass
  , OldBookOfLore' <$> oldBookOfLore
  , ResearchLibrarian' <$> researchLibrarian
  , DrMilanChristopher' <$> drMilanChristopher
  , Hyperawareness' <$> hyperawareness
  , MedicalTexts' <$> medicalTexts
  , MagnifyingGlass1' <$> magnifyingGlass1
  , DiscOfItzamna2' <$> discOfItzamna2
  , Encyclopedia2' <$> encyclopedia2
  , Switchblade' <$> switchblade
  , Burglary' <$> burglary
  , Pickpocketing' <$> pickpoketing
  , FortyOneDerringer' <$> fortyOneDerringer
  , LeoDeLuca' <$> leoDeLuca
  , HardKnocks' <$> hardKnocks
  , LeoDeLuca1' <$> leoDeLuca1
  , CatBurglar1' <$> catBurglar1
  , ForbiddenKnowledge' <$> forbiddenKnowledge
  , HolyRosary' <$> holyRosary
  , Shrivelling' <$> shrivelling
  , Scrying' <$> scrying
  , ArcaneStudies' <$> arcaneStudies
  , ArcaneInitiate' <$> arcaneInitiate
  , BookOfShadows3' <$> bookOfShadows3
  , GrotesqueStatue4' <$> grotesqueStatue4
  , LeatherCoat' <$> leatherCoat
  , Scavenging' <$> scavenging
  , BaseballBat' <$> baseballBat
  , RabbitsFoot' <$> rabbitsFoot
  , StrayCat' <$> strayCat
  , DigDeep' <$> digDeep
  , Aquinnah1' <$> aquinnah1
  , Knife' <$> knife
  , Flashlight' <$> flashlight
  , BulletproofVest3' <$> bulletproofVest3
  , ElderSignAmulet3' <$> elderSignAmulet3
  , LitaChantler' <$> litaChantler
  , ZoeysCross' <$> zoeysCross
  , JennysTwin45s' <$> jennysTwin45s
  , JimsTrumpet' <$> jimsTrumpet
  , Duke' <$> duke
  , Blackjack' <$> blackjack
  , LaboratoryAssistant' <$> laboratoryAssistant
  , StrangeSolution' <$> strangeSolution
  , LiquidCourage' <$> liquidCourage
  , HiredMuscle1' <$> hiredMuscle1
  , RiteOfSeeking' <$> riteOfSeeking
  , RitualCandles' <$> ritualCandles
  , ClarityOfMind' <$> clarityOfMind
  , FireAxe' <$> fireAxe
  , PeterSylvestre' <$> peterSylvestre
  , PeterSylvestre2' <$> peterSylvestre2
  , Kukri' <$> kukri
  , DrHenryArmitage' <$> drHenryArmitage
  , AlchemicalConcoction' <$> alchemicalConcoction
  , JazzMulligan' <$> jazzMulligan
  , ProfessorWarrenRice' <$> professorWarrenRice
  , PeterClover' <$> peterClover
  , DrFrancisMorgan' <$> drFrancisMorgan
  , BrotherXavier1' <$> brotherXavier1
  , Pathfinder1' <$> pathfinder1
  , Adaptable1' <$> adaptable1
  , SongOfTheDead2' <$> songOfTheDead2
  , HaroldWalsted' <$> haroldWalsted
  , AdamLynch' <$> adamLynch
  , TheNecronomiconOlausWormiusTranslation'
    <$> theNecronomiconOlausWormiusTranslation
  , Bandolier' <$> bandolier
  , HelplessPassenger' <$> helplessPassenger
  , KeenEye3' <$> keenEye3
  , KeyToTheChamber' <$> keyToTheChamber
  , ZebulonWhateley' <$> zebulonWhateley
  , EarlSawyer' <$> earlSawyer
  , PowderOfIbnGhazi' <$> powderOfIbnGhazi
  , SpringfieldM19034' <$> springfieldM19034
  , EsotericFormula' <$> esotericFormula
  , LightningGun5' <$> lightningGun5
  , ToothOfEztli' <$> toothOfEztli
  , OccultLexicon' <$> occultLexicon
  , ScrollOfProphecies' <$> scrollOfProphecies
  , KeenEye' <$> keenEye
  , PhysicalTraining2' <$> physicalTraining2
  , Hyperawareness2' <$> hyperawareness2
  , HardKnocks2' <$> hardKnocks2
  , ArcaneStudies2' <$> arcaneStudies2
  , DigDeep2' <$> digDeep2
  , RabbitsFoot3' <$> rabbitsFoot3
  , ArcaneEnlightenment' <$> arcaneEnlightenment
  , CelaenoFragments' <$> celaenoFragments
  , Encyclopedia' <$> encyclopedia
  , HigherEducation' <$> higherEducation
  , WhittonGreene' <$> whittonGreene
  , LadyEsprit' <$> ladyEsprit
  , BearTrap' <$> bearTrap
  , FishingNet' <$> fishingNet
  , MonstrousTransformation' <$> monstrousTransformation
  , DaisysToteBagAdvanced' <$> daisysToteBagAdvanced
  , TheNecronomiconAdvanced' <$> theNecronomiconAdvanced
  , CardBuilder
    { cbCardCode = "asset"
    , cbCardBuilder = \aid -> baseAsset aid "asset" id id
    }
  ]

instance IsAsset Asset where
  slotsOf = slotsOf . toAttrs
  useTypeOf = useTypeOf . toAttrs
  isHealthDamageable = isHealthDamageable . toAttrs
  isSanityDamageable = isSanityDamageable . toAttrs
  isStory = isStory . toAttrs
