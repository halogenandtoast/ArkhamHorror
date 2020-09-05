{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset
  ( lookupAsset
  , allAssets
  , isHealthDamageable
  , isSanityDamageable
  , slotsOf
  , Asset
  )
where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Cards.ArcaneInitiate
import Arkham.Types.Asset.Cards.ArcaneStudies
import Arkham.Types.Asset.Cards.ArcaneStudies2
import Arkham.Types.Asset.Cards.BaseballBat
import Arkham.Types.Asset.Cards.BeatCop
import Arkham.Types.Asset.Cards.BeatCop2
import Arkham.Types.Asset.Cards.Burglary
import Arkham.Types.Asset.Cards.DaisysToteBag
import Arkham.Types.Asset.Cards.DigDeep
import Arkham.Types.Asset.Cards.DigDeep2
import Arkham.Types.Asset.Cards.DrMilanChristopher
import Arkham.Types.Asset.Cards.FirstAid
import Arkham.Types.Asset.Cards.Flashlight
import Arkham.Types.Asset.Cards.ForbiddenKnowledge
import Arkham.Types.Asset.Cards.FortyFiveAutomatic
import Arkham.Types.Asset.Cards.FortyOneDerringer
import Arkham.Types.Asset.Cards.GuardDog
import Arkham.Types.Asset.Cards.HardKnocks
import Arkham.Types.Asset.Cards.HardKnocks2
import Arkham.Types.Asset.Cards.HeirloomOfHyperborea
import Arkham.Types.Asset.Cards.HolyRosary
import Arkham.Types.Asset.Cards.Hyperawareness
import Arkham.Types.Asset.Cards.Hyperawareness2
import Arkham.Types.Asset.Cards.Knife
import Arkham.Types.Asset.Cards.LeatherCoat
import Arkham.Types.Asset.Cards.LeoDeLuca
import Arkham.Types.Asset.Cards.LitaChantler
import Arkham.Types.Asset.Cards.Machete
import Arkham.Types.Asset.Cards.MagnifyingGlass
import Arkham.Types.Asset.Cards.MagnifyingGlass1
import Arkham.Types.Asset.Cards.MedicalTexts
import Arkham.Types.Asset.Cards.OldBookOfLore
import Arkham.Types.Asset.Cards.PhysicalTraining
import Arkham.Types.Asset.Cards.PhysicalTraining2
import Arkham.Types.Asset.Cards.Pickpocketing
import Arkham.Types.Asset.Cards.PoliceBadge2
import Arkham.Types.Asset.Cards.RabbitsFoot
import Arkham.Types.Asset.Cards.RabbitsFoot3
import Arkham.Types.Asset.Cards.ResearchLibrarian
import Arkham.Types.Asset.Cards.Rolands38Special
import Arkham.Types.Asset.Cards.Scavenging
import Arkham.Types.Asset.Cards.Scrying
import Arkham.Types.Asset.Cards.Shotgun4
import Arkham.Types.Asset.Cards.Shrivelling
import Arkham.Types.Asset.Cards.StrayCat
import Arkham.Types.Asset.Cards.Switchblade
import Arkham.Types.Asset.Cards.TheNecronomicon
import Arkham.Types.Asset.Cards.WendysAmulet
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Slot
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupAsset :: CardCode -> (AssetId -> Asset)
lookupAsset = fromJustNote "Unkown asset" . flip HashMap.lookup allAssets

allAssets :: HashMap CardCode (AssetId -> Asset)
allAssets = HashMap.fromList
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
  , ("01044", Switchblade' . switchblade)
  , ("01045", Burglary' . burglary)
  , ("01046", Pickpocketing' . pickpoketing)
  , ("01047", FortyOneDerringer' . fortyOneDerringer)
  , ("01048", LeoDeLuca' . leoDeLuca)
  , ("01049", HardKnocks' . hardKnocks)
  , ("01058", ForbiddenKnowledge' . forbiddenKnowledge)
  , ("01059", HolyRosary' . holyRosary)
  , ("01060", Shrivelling' . shrivelling)
  , ("01061", Scrying' . scrying)
  , ("01062", ArcaneStudies' . arcaneStudies)
  , ("01063", ArcaneInitiate' . arcaneInitiate)
  , ("01072", LeatherCoat' . leatherCoat)
  , ("01073", Scavenging' . scavenging)
  , ("01074", BaseballBat' . baseballBat)
  , ("01075", RabbitsFoot' . rabbitsFoot)
  , ("01076", StrayCat' . strayCat)
  , ("01077", DigDeep' . digDeep)
  , ("01086", Knife' . knife)
  , ("01087", Flashlight' . flashlight)
  , ("01117", LitaChantler' . litaChantler)
  , ("50001", PhysicalTraining2' . physicalTraining2)
  , ("50003", Hyperawareness2' . hyperawareness2)
  , ("50005", HardKnocks2' . hardKnocks2)
  , ("50007", ArcaneStudies2' . arcaneStudies2)
  , ("50009", DigDeep2' . digDeep2)
  , ("50010", RabbitsFoot3' . rabbitsFoot3)
  ]

instance HasCardCode Asset where
  getCardCode = assetCardCode . assetAttrs

instance HasTraits Asset where
  getTraits = assetTraits . assetAttrs

instance HasId (Maybe OwnerId) () Asset where
  getId _ = (OwnerId <$>) . assetInvestigator . assetAttrs

instance HasId (Maybe LocationId) () Asset where
  getId _ = assetLocation . assetAttrs

instance HasCount DoomCount () Asset where
  getCount _ = DoomCount . assetDoom . assetAttrs

slotsOf :: Asset -> [SlotType]
slotsOf = assetSlots . assetAttrs

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
  | Switchblade' Switchblade
  | Burglary' Burglary
  | Pickpocketing' Pickpocketing
  | FortyOneDerringer' FortyOneDerringer
  | LeoDeLuca' LeoDeLuca
  | HardKnocks' HardKnocks
  | ForbiddenKnowledge' ForbiddenKnowledge
  | HolyRosary' HolyRosary
  | Shrivelling' Shrivelling
  | Scrying' Scrying
  | ArcaneStudies' ArcaneStudies
  | ArcaneInitiate' ArcaneInitiate
  | LeatherCoat' LeatherCoat
  | Scavenging' Scavenging
  | BaseballBat' BaseballBat
  | RabbitsFoot' RabbitsFoot
  | StrayCat' StrayCat
  | DigDeep' DigDeep
  | Knife' Knife
  | Flashlight' Flashlight
  | LitaChantler' LitaChantler
  | PhysicalTraining2' PhysicalTraining2
  | Hyperawareness2' Hyperawareness2
  | HardKnocks2' HardKnocks2
  | ArcaneStudies2' ArcaneStudies2
  | DigDeep2' DigDeep2
  | RabbitsFoot3' RabbitsFoot3
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

assetAttrs :: Asset -> Attrs
assetAttrs = \case
  Rolands38Special' attrs -> coerce attrs
  DaisysToteBag' attrs -> coerce attrs
  TheNecronomicon' (TheNecronomicon (attrs `With` _)) -> attrs
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
  Switchblade' attrs -> coerce attrs
  Burglary' attrs -> coerce attrs
  Pickpocketing' attrs -> coerce attrs
  FortyOneDerringer' attrs -> coerce attrs
  LeoDeLuca' attrs -> coerce attrs
  HardKnocks' attrs -> coerce attrs
  ForbiddenKnowledge' attrs -> coerce attrs
  HolyRosary' attrs -> coerce attrs
  Shrivelling' attrs -> coerce attrs
  Scrying' attrs -> coerce attrs
  ArcaneStudies' attrs -> coerce attrs
  ArcaneInitiate' attrs -> coerce attrs
  LeatherCoat' attrs -> coerce attrs
  Scavenging' attrs -> coerce attrs
  BaseballBat' attrs -> coerce attrs
  RabbitsFoot' attrs -> coerce attrs
  StrayCat' attrs -> coerce attrs
  DigDeep' attrs -> coerce attrs
  Knife' attrs -> coerce attrs
  Flashlight' attrs -> coerce attrs
  LitaChantler' attrs -> coerce attrs
  PhysicalTraining2' attrs -> coerce attrs
  Hyperawareness2' attrs -> coerce attrs
  HardKnocks2' attrs -> coerce attrs
  ArcaneStudies2' attrs -> coerce attrs
  DigDeep2' attrs -> coerce attrs
  RabbitsFoot3' attrs -> coerce attrs

isHealthDamageable :: Asset -> Bool
isHealthDamageable a = case assetHealth (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetHealthDamage (assetAttrs a)

isSanityDamageable :: Asset -> Bool
isSanityDamageable a = case assetSanity (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetSanityDamage (assetAttrs a)

instance (ActionRunner env investigator) => HasActions env investigator Asset where
  getActions i window = \case
    Rolands38Special' x -> getActions i window x
    DaisysToteBag' x -> getActions i window x
    TheNecronomicon' x -> getActions i window x
    HeirloomOfHyperborea' x -> getActions i window x
    WendysAmulet' x -> getActions i window x
    FortyFiveAutomatic' x -> getActions i window x
    PhysicalTraining' x -> getActions i window x
    BeatCop' x -> getActions i window x
    FirstAid' x -> getActions i window x
    Machete' x -> getActions i window x
    GuardDog' x -> getActions i window x
    PoliceBadge2' x -> getActions i window x
    BeatCop2' x -> getActions i window x
    Shotgun4' x -> getActions i window x
    MagnifyingGlass' x -> getActions i window x
    OldBookOfLore' x -> getActions i window x
    ResearchLibrarian' x -> getActions i window x
    DrMilanChristopher' x -> getActions i window x
    MedicalTexts' x -> getActions i window x
    MagnifyingGlass1' x -> getActions i window x
    Hyperawareness' x -> getActions i window x
    Switchblade' x -> getActions i window x
    Burglary' x -> getActions i window x
    Pickpocketing' x -> getActions i window x
    FortyOneDerringer' x -> getActions i window x
    LeoDeLuca' x -> getActions i window x
    HardKnocks' x -> getActions i window x
    ForbiddenKnowledge' x -> getActions i window x
    HolyRosary' x -> getActions i window x
    Shrivelling' x -> getActions i window x
    Scrying' x -> getActions i window x
    ArcaneStudies' x -> getActions i window x
    ArcaneInitiate' x -> getActions i window x
    LeatherCoat' x -> getActions i window x
    Scavenging' x -> getActions i window x
    BaseballBat' x -> getActions i window x
    RabbitsFoot' x -> getActions i window x
    StrayCat' x -> getActions i window x
    DigDeep' x -> getActions i window x
    Knife' x -> getActions i window x
    Flashlight' x -> getActions i window x
    LitaChantler' x -> getActions i window x
    PhysicalTraining2' x -> getActions i window x
    Hyperawareness2' x -> getActions i window x
    HardKnocks2' x -> getActions i window x
    ArcaneStudies2' x -> getActions i window x
    DigDeep2' x -> getActions i window x
    RabbitsFoot3' x -> getActions i window x

instance (AssetRunner env) => RunMessage env Asset where
  runMessage msg = \case
    Rolands38Special' x -> Rolands38Special' <$> runMessage msg x
    DaisysToteBag' x -> DaisysToteBag' <$> runMessage msg x
    TheNecronomicon' x -> TheNecronomicon' <$> runMessage msg x
    HeirloomOfHyperborea' x -> HeirloomOfHyperborea' <$> runMessage msg x
    WendysAmulet' x -> WendysAmulet' <$> runMessage msg x
    FortyFiveAutomatic' x -> FortyFiveAutomatic' <$> runMessage msg x
    PhysicalTraining' x -> PhysicalTraining' <$> runMessage msg x
    BeatCop' x -> BeatCop' <$> runMessage msg x
    FirstAid' x -> FirstAid' <$> runMessage msg x
    Machete' x -> Machete' <$> runMessage msg x
    GuardDog' x -> GuardDog' <$> runMessage msg x
    PoliceBadge2' x -> PoliceBadge2' <$> runMessage msg x
    BeatCop2' x -> BeatCop2' <$> runMessage msg x
    Shotgun4' x -> Shotgun4' <$> runMessage msg x
    MagnifyingGlass' x -> MagnifyingGlass' <$> runMessage msg x
    OldBookOfLore' x -> OldBookOfLore' <$> runMessage msg x
    ResearchLibrarian' x -> ResearchLibrarian' <$> runMessage msg x
    DrMilanChristopher' x -> DrMilanChristopher' <$> runMessage msg x
    Hyperawareness' x -> Hyperawareness' <$> runMessage msg x
    MedicalTexts' x -> MedicalTexts' <$> runMessage msg x
    MagnifyingGlass1' x -> MagnifyingGlass1' <$> runMessage msg x
    Switchblade' x -> Switchblade' <$> runMessage msg x
    Burglary' x -> Burglary' <$> runMessage msg x
    Pickpocketing' x -> Pickpocketing' <$> runMessage msg x
    FortyOneDerringer' x -> FortyOneDerringer' <$> runMessage msg x
    LeoDeLuca' x -> LeoDeLuca' <$> runMessage msg x
    HardKnocks' x -> HardKnocks' <$> runMessage msg x
    ForbiddenKnowledge' x -> ForbiddenKnowledge' <$> runMessage msg x
    HolyRosary' x -> HolyRosary' <$> runMessage msg x
    Shrivelling' x -> Shrivelling' <$> runMessage msg x
    Scrying' x -> Scrying' <$> runMessage msg x
    ArcaneStudies' x -> ArcaneStudies' <$> runMessage msg x
    ArcaneInitiate' x -> ArcaneInitiate' <$> runMessage msg x
    LeatherCoat' x -> LeatherCoat' <$> runMessage msg x
    BaseballBat' x -> BaseballBat' <$> runMessage msg x
    Scavenging' x -> Scavenging' <$> runMessage msg x
    RabbitsFoot' x -> RabbitsFoot' <$> runMessage msg x
    StrayCat' x -> StrayCat' <$> runMessage msg x
    DigDeep' x -> DigDeep' <$> runMessage msg x
    Knife' x -> Knife' <$> runMessage msg x
    Flashlight' x -> Flashlight' <$> runMessage msg x
    LitaChantler' x -> LitaChantler' <$> runMessage msg x
    PhysicalTraining2' x -> PhysicalTraining2' <$> runMessage msg x
    Hyperawareness2' x -> Hyperawareness2' <$> runMessage msg x
    HardKnocks2' x -> HardKnocks2' <$> runMessage msg x
    ArcaneStudies2' x -> ArcaneStudies2' <$> runMessage msg x
    DigDeep2' x -> DigDeep2' <$> runMessage msg x
    RabbitsFoot3' x -> RabbitsFoot3' <$> runMessage msg x
