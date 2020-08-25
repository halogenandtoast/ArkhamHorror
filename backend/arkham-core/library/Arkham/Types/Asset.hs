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
import Arkham.Types.Asset.Cards.BeatCop
import Arkham.Types.Asset.Cards.DaisysToteBag
import Arkham.Types.Asset.Cards.DrMilanChristopher
import Arkham.Types.Asset.Cards.Flashlight
import Arkham.Types.Asset.Cards.FortyFiveAutomatic
import Arkham.Types.Asset.Cards.GuardDog
import Arkham.Types.Asset.Cards.HeirloomOfHyperborea
import Arkham.Types.Asset.Cards.HolyRosary
import Arkham.Types.Asset.Cards.Knife
import Arkham.Types.Asset.Cards.LeatherCoat
import Arkham.Types.Asset.Cards.LitaChantler
import Arkham.Types.Asset.Cards.Machete
import Arkham.Types.Asset.Cards.MagnifyingGlass
import Arkham.Types.Asset.Cards.MedicalTexts
import Arkham.Types.Asset.Cards.OldBookOfLore
import Arkham.Types.Asset.Cards.PhysicalTraining
import Arkham.Types.Asset.Cards.ResearchLibrarian
import Arkham.Types.Asset.Cards.Rolands38Special
import Arkham.Types.Asset.Cards.Scrying
import Arkham.Types.Asset.Cards.Shrivelling
import Arkham.Types.Asset.Cards.TheNecronomicon
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
  , ("01016", FortyFiveAutomatic' . fortyFiveAutomatic)
  , ("01017", PhysicalTraining' . physicalTraining)
  , ("01018", BeatCop' . beatCop)
  , ("01020", Machete' . machete)
  , ("01021", GuardDog' . guardDog)
  , ("01030", MagnifyingGlass' . magnifyingGlass)
  , ("01031", OldBookOfLore' . oldBookOfLore)
  , ("01032", ResearchLibrarian' . researchLibrarian)
  , ("01033", DrMilanChristopher' . drMilanChristopher)
  , ("01035", MedicalTexts' . medicalTexts)
  , ("01059", HolyRosary' . holyRosary)
  , ("01060", Shrivelling' . shrivelling)
  , ("01061", Scrying' . scrying)
  , ("01072", LeatherCoat' . leatherCoat)
  , ("01086", Knife' . knife)
  , ("01087", Flashlight' . flashlight)
  , ("01117", LitaChantler' . litaChantler)
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
  | FortyFiveAutomatic' FortyFiveAutomatic
  | PhysicalTraining' PhysicalTraining
  | BeatCop' BeatCop
  | Machete' Machete
  | GuardDog' GuardDog
  | MagnifyingGlass' MagnifyingGlass
  | OldBookOfLore' OldBookOfLore
  | MedicalTexts' MedicalTexts
  | ResearchLibrarian' ResearchLibrarian
  | DrMilanChristopher' DrMilanChristopher
  | HolyRosary' HolyRosary
  | Shrivelling' Shrivelling
  | Scrying' Scrying
  | LeatherCoat' LeatherCoat
  | Knife' Knife
  | Flashlight' Flashlight
  | LitaChantler' LitaChantler
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

assetAttrs :: Asset -> Attrs
assetAttrs = \case
  Rolands38Special' attrs -> coerce attrs
  DaisysToteBag' attrs -> coerce attrs
  TheNecronomicon' (TheNecronomicon (attrs `With` _)) -> attrs
  HeirloomOfHyperborea' attrs -> coerce attrs
  FortyFiveAutomatic' attrs -> coerce attrs
  PhysicalTraining' attrs -> coerce attrs
  BeatCop' attrs -> coerce attrs
  Machete' attrs -> coerce attrs
  GuardDog' attrs -> coerce attrs
  MagnifyingGlass' attrs -> coerce attrs
  OldBookOfLore' attrs -> coerce attrs
  MedicalTexts' attrs -> coerce attrs
  ResearchLibrarian' attrs -> coerce attrs
  DrMilanChristopher' attrs -> coerce attrs
  HolyRosary' attrs -> coerce attrs
  Shrivelling' attrs -> coerce attrs
  Scrying' attrs -> coerce attrs
  LeatherCoat' attrs -> coerce attrs
  Knife' attrs -> coerce attrs
  Flashlight' attrs -> coerce attrs
  LitaChantler' attrs -> coerce attrs

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
    FortyFiveAutomatic' x -> getActions i window x
    PhysicalTraining' x -> getActions i window x
    BeatCop' x -> getActions i window x
    Machete' x -> getActions i window x
    GuardDog' x -> getActions i window x
    MagnifyingGlass' x -> getActions i window x
    OldBookOfLore' x -> getActions i window x
    MedicalTexts' x -> getActions i window x
    ResearchLibrarian' x -> getActions i window x
    DrMilanChristopher' x -> getActions i window x
    HolyRosary' x -> getActions i window x
    Shrivelling' x -> getActions i window x
    Scrying' x -> getActions i window x
    LeatherCoat' x -> getActions i window x
    Knife' x -> getActions i window x
    Flashlight' x -> getActions i window x
    LitaChantler' x -> getActions i window x

instance (AssetRunner env) => RunMessage env Asset where
  runMessage msg = \case
    Rolands38Special' x -> Rolands38Special' <$> runMessage msg x
    DaisysToteBag' x -> DaisysToteBag' <$> runMessage msg x
    TheNecronomicon' x -> TheNecronomicon' <$> runMessage msg x
    HeirloomOfHyperborea' x -> HeirloomOfHyperborea' <$> runMessage msg x
    FortyFiveAutomatic' x -> FortyFiveAutomatic' <$> runMessage msg x
    PhysicalTraining' x -> PhysicalTraining' <$> runMessage msg x
    BeatCop' x -> BeatCop' <$> runMessage msg x
    Machete' x -> Machete' <$> runMessage msg x
    GuardDog' x -> GuardDog' <$> runMessage msg x
    MagnifyingGlass' x -> MagnifyingGlass' <$> runMessage msg x
    OldBookOfLore' x -> OldBookOfLore' <$> runMessage msg x
    MedicalTexts' x -> MedicalTexts' <$> runMessage msg x
    ResearchLibrarian' x -> ResearchLibrarian' <$> runMessage msg x
    DrMilanChristopher' x -> DrMilanChristopher' <$> runMessage msg x
    HolyRosary' x -> HolyRosary' <$> runMessage msg x
    Shrivelling' x -> Shrivelling' <$> runMessage msg x
    Scrying' x -> Scrying' <$> runMessage msg x
    LeatherCoat' x -> LeatherCoat' <$> runMessage msg x
    Knife' x -> Knife' <$> runMessage msg x
    Flashlight' x -> Flashlight' <$> runMessage msg x
    LitaChantler' x -> LitaChantler' <$> runMessage msg x
