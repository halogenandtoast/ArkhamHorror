module Arkham.Customization where

import Arkham.Prelude
import Arkham.SkillType (SkillType)
import Arkham.Trait (Trait)

data Customization
  = -- placeholder, must be the top entry because the map has to be ordered
    ChoicePlaceholder
  | -- Hunter's Armor 09021
    Enchanted -- 0
  | ProtectiveRunes -- 1
  | Durable -- 2
  | Hallowed -- 3
  | Lightweight -- 4
  | Hexdrinker -- 5
  | ArmorOfThorns -- 6
  | -- Runic Axe 09022
    Heirloom -- 0
  | InscriptionOfGlory -- 1
  | InscriptionOfTheElders -- 2
  | InscriptionOfTheHunt -- 3
  | InscriptionOfFury -- 4
  | AncientPower -- 5
  | Saga -- 6
  | Scriptweaver -- 7
  | -- Custom Modifications 09023
    NotchedSight -- 0
  | ExtendedStock -- 1
  | Counterbalance -- 2
  | LeatherGrip -- 3
  | ExtendedMagazine -- 4
  | QuicksilverBullets -- 5
  | -- Alchemical Distillation 09040
    MendingDistillate -- 0
  | CalmingDistillate -- 1
  | EnlighteningDistillate -- 2
  | QuickeningDistillate -- 3
  | Refined -- 4
  | Empowered -- 5
  | Perfected -- 6
  | -- Empirical Hypothesis 09041
    PessimisticOutlook -- 0
  | TrialAndError -- 1
  | IndepedentVariable -- 2
  | FieldResearch -- 3
  | PeerReview -- 4
  | ResearchGrant -- 5
  | IrrefutableProof -- 6
  | AlternativeHypothesis -- 7
  | -- The Raven Quill 09042
    -- 0 is ChoicePlaceholder
    LivingQuill -- 1
  | SpectralBinding -- 2
  | MysticVane -- 3
  | EndlessInkwell -- 4
  | EnergySap -- 5
  | InterwovenInk -- 6
  | SupernaturalRecord -- 7
  | -- Damning Testimony 09059
    SearchWarrant -- 0
  | FabricatedEvidence -- 1
  | Blackmail -- 2
  | Extort -- 3
  | Surveil -- 4
  | Expose -- 5
  | -- Hyperphysical Shotcaster 09119
    Railshooter -- 0
  | Telescanner -- 1
  | Translocator -- 2
  | Realitycollapser -- 3
  | Matterweaver -- 4
  | AethericLink -- 5
  | EmpoweredConfiguration -- 6
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data CustomizationChoice = ChosenCard Text | ChosenSkill SkillType | ChosenTrait Trait
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

type Customizations = IntMap (Int, [CustomizationChoice])
