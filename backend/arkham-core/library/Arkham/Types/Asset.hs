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
import Arkham.Types.Asset.Cards.Bandolier
import Arkham.Types.Asset.Cards.BaseballBat
import Arkham.Types.Asset.Cards.BeatCop
import Arkham.Types.Asset.Cards.BeatCop2
import Arkham.Types.Asset.Cards.BookOfShadows3
import Arkham.Types.Asset.Cards.Burglary
import Arkham.Types.Asset.Cards.CatBurgler1
import Arkham.Types.Asset.Cards.DaisysToteBag
import Arkham.Types.Asset.Cards.DigDeep
import Arkham.Types.Asset.Cards.DigDeep2
import Arkham.Types.Asset.Cards.DiscOfItzamna2
import Arkham.Types.Asset.Cards.DrMilanChristopher
import Arkham.Types.Asset.Cards.Encyclopedia2
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
import Arkham.Types.Asset.Cards.LeoDeLuca1
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
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Query
import Arkham.Types.Slot
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
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
  , ("01072", LeatherCoat' . leatherCoat)
  , ("01073", Scavenging' . scavenging)
  , ("01074", BaseballBat' . baseballBat)
  , ("01075", RabbitsFoot' . rabbitsFoot)
  , ("01076", StrayCat' . strayCat)
  , ("01077", DigDeep' . digDeep)
  , ("01086", Knife' . knife)
  , ("01087", Flashlight' . flashlight)
  , ("01117", LitaChantler' . litaChantler)
  , ("02147", Bandolier' . bandolier)
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
  | LeatherCoat' LeatherCoat
  | Scavenging' Scavenging
  | BaseballBat' BaseballBat
  | RabbitsFoot' RabbitsFoot
  | StrayCat' StrayCat
  | DigDeep' DigDeep
  | Knife' Knife
  | Flashlight' Flashlight
  | LitaChantler' LitaChantler
  | Bandolier' Bandolier
  | PhysicalTraining2' PhysicalTraining2
  | Hyperawareness2' Hyperawareness2
  | HardKnocks2' HardKnocks2
  | ArcaneStudies2' ArcaneStudies2
  | DigDeep2' DigDeep2
  | RabbitsFoot3' RabbitsFoot3
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Asset
deriving anyclass instance (AssetRunner env) => RunMessage env Asset

assetAttrs :: Asset -> Attrs
assetAttrs = getAttrs

isHealthDamageable :: Asset -> Bool
isHealthDamageable a = case assetHealth (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetHealthDamage (assetAttrs a)

isSanityDamageable :: Asset -> Bool
isSanityDamageable a = case assetSanity (assetAttrs a) of
  Nothing -> False
  Just n -> n > assetSanityDamage (assetAttrs a)

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"
