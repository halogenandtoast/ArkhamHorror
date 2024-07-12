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
  | -- Friends in Low Places 09060
    -- 0 is ChoicePlaceholder
    Helpful -- 1
  | Versatile -- 2
  | Bolstering -- 3
  | Clever -- 4
  | Prompt -- 5
  | Experienced -- 6
  | Swift -- 7
  | -- Honed Instinct 09061
    ReflexResponse -- 0
  | SituationalAwareness -- 1
  | KillerInstinct -- 2
  | GutReaction -- 3
  | MuscleMemory -- 4
  | SharpenedTalent -- 5
  | ImpulseControl -- 6
  | ForceOfHabit -- 7
  | -- Living Ink 09079
    -- 0 is ChoicePlaceholder
    ShiftingInk -- 1
  | SubtleDepiction -- 2
  | ImbuedInk -- 3
  | EldritchInk -- 4
  | EldritchInk2 -- 5
  | MacabreDepiction -- 6
  | Vibrancy -- 7
  | -- Summoned Servitor 09080
    ArmoredCarapace -- 0
  | ClawsThatCatch -- 1
  | JawsThatSnatch -- 2
  | EyesOfFlame -- 3
  | WingsOfNight -- 4
  | Dominance -- 5
  | DreamingCall -- 6
  | DæmonicInfluence -- 7
  | -- Power Word 09081
    Betray -- 0
  | Mercy -- 1
  | Confess -- 2
  | Distract -- 3
  | GreaterControl -- 4
  | Bonded -- 5
  | Tonguetwister -- 6
  | ThriceSpoken -- 7
  | -- Pocket Multi Tool 09099
    Detachable -- 0
  | PryBar -- 1
  | SharpenedKnife -- 2
  | SignalMirror -- 3
  | MagnifyingLens -- 4
  | LuckyCharm -- 5
  | SpringLoaded -- 6
  | -- Makeshift Trap 09100
    ImprovedTimer -- 0
  | Tripwire -- 1
  | Simple -- 2
  | Poisonous -- 3
  | RemoteConfiguration -- 4
  | Net -- 5
  | ExplosiveDevice -- 6
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

data CustomizationChoice = ChosenCard Text | ChosenSkill SkillType | ChosenTrait Trait | ChosenIndex Int
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

type Customizations = IntMap (Int, [CustomizationChoice])
