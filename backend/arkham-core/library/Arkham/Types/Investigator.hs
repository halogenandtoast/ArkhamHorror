{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator
  ( isPrey
  , hasEndedTurn
  , hasResigned
  , hasClues
  , isDefeated
  , remainingHealth
  , lookupInvestigator
  , handOf
  , deckOf
  , availableSkillsFor
  , GetInvestigatorId(..)
  , Investigator
  )
where

import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards.AgnesBaker
import Arkham.Types.Investigator.Cards.AkachiOnyele
import Arkham.Types.Investigator.Cards.AmandaSharpe
import Arkham.Types.Investigator.Cards.AshcanPete
import Arkham.Types.Investigator.Cards.CalvinWright
import Arkham.Types.Investigator.Cards.CarolynFern
import Arkham.Types.Investigator.Cards.DaisyWalker
import Arkham.Types.Investigator.Cards.DexterDrake
import Arkham.Types.Investigator.Cards.DianaStanley
import Arkham.Types.Investigator.Cards.FatherMateo
import Arkham.Types.Investigator.Cards.FinnEdwards
import Arkham.Types.Investigator.Cards.HarveyWalters
import Arkham.Types.Investigator.Cards.JacquelineFine
import Arkham.Types.Investigator.Cards.JennyBarnes
import Arkham.Types.Investigator.Cards.JimCulver
import Arkham.Types.Investigator.Cards.JoeDiamond
import Arkham.Types.Investigator.Cards.LeoAnderson
import Arkham.Types.Investigator.Cards.LolaHayes
import Arkham.Types.Investigator.Cards.LukeRobinson
import Arkham.Types.Investigator.Cards.MandyThompson
import Arkham.Types.Investigator.Cards.MarieLambeau
import Arkham.Types.Investigator.Cards.MarkHarrigan
import Arkham.Types.Investigator.Cards.MinhThiPhan
import Arkham.Types.Investigator.Cards.NathanielCho
import Arkham.Types.Investigator.Cards.NormanWithers
import Arkham.Types.Investigator.Cards.PatriceHathaway
import Arkham.Types.Investigator.Cards.PrestonFairmont
import Arkham.Types.Investigator.Cards.RexMurphy
import Arkham.Types.Investigator.Cards.RitaYoung
import Arkham.Types.Investigator.Cards.RolandBanks
import Arkham.Types.Investigator.Cards.SefinaRousseau
import Arkham.Types.Investigator.Cards.SilasMarsh
import Arkham.Types.Investigator.Cards.SisterMary
import Arkham.Types.Investigator.Cards.SkidsOToole
import Arkham.Types.Investigator.Cards.StellaClark
import Arkham.Types.Investigator.Cards.TommyMuldoon
import Arkham.Types.Investigator.Cards.TonyMorgan
import Arkham.Types.Investigator.Cards.UrsulaDowns
import Arkham.Types.Investigator.Cards.WendyAdams
import Arkham.Types.Investigator.Cards.WilliamYorick
import Arkham.Types.Investigator.Cards.WinifredHabbamock
import Arkham.Types.Investigator.Cards.ZoeySamaras
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Stats
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Lens.Micro.Extras
import Safe (fromJustNote)

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = HashMap.fromList $ map
  (\s -> (investigatorId . investigatorAttrs $ s, s))
  [ AgnesBaker' agnesBaker
  , AkachiOnyele' akachiOnyele
  , AmandaSharpe' amandaSharpe
  , AshcanPete' ashcanPete
  , CalvinWright' calvinWright
  , CarolynFern' carolynFern
  , DaisyWalker' daisyWalker
  , DexterDrake' dexterDrake
  , DianaStanley' dianaStanley
  , FatherMateo' fatherMateo
  , FinnEdwards' finnEdwards
  , HarveyWalters' harveyWalters
  , JacquelineFine' jacquelineFine
  , JennyBarnes' jennyBarnes
  , JimCulver' jimCulver
  , JoeDiamond' joeDiamond
  , LeoAnderson' leoAnderson
  , LolaHayes' lolaHayes
  , LukeRobinson' lukeRobinson
  , MandyThompson' mandyThompson
  , MarieLambeau' marieLambeau
  , MarkHarrigan' markHarrigan
  , MinhThiPhan' minhThiPhan
  , NathanielCho' nathanielCho
  , NormanWithers' normanWithers
  , PatriceHathaway' patriceHathaway
  , PrestonFairmont' prestonFairmont
  , RexMurphy' rexMurphy
  , RitaYoung' ritaYoung
  , RolandBanks' rolandBanks
  , SefinaRousseau' sefinaRousseau
  , SilasMarsh' silasMarsh
  , SisterMary' sisterMary
  , SkidsOToole' skidsOToole
  , StellaClark' stellaClark
  , TommyMuldoon' tommyMuldoon
  , TonyMorgan' tonyMorgan
  , UrsulaDowns' ursulaDowns
  , WendyAdams' wendyAdams
  , WilliamYorick' williamYorick
  , WinifredHabbamock' winifredHabbamock
  , ZoeySamaras' zoeySamaras
  ]

investigatorAttrs :: Investigator -> Attrs
investigatorAttrs = \case
  AgnesBaker' attrs -> coerce attrs
  AkachiOnyele' attrs -> coerce attrs
  AmandaSharpe' attrs -> coerce attrs
  AshcanPete' attrs -> coerce attrs
  CalvinWright' attrs -> coerce attrs
  CarolynFern' attrs -> coerce attrs
  DaisyWalker' (DaisyWalker (With attrs _)) -> attrs
  DexterDrake' attrs -> coerce attrs
  DianaStanley' attrs -> coerce attrs
  FatherMateo' attrs -> coerce attrs
  FinnEdwards' attrs -> coerce attrs
  HarveyWalters' attrs -> coerce attrs
  JacquelineFine' attrs -> coerce attrs
  JennyBarnes' attrs -> coerce attrs
  JimCulver' attrs -> coerce attrs
  JoeDiamond' attrs -> coerce attrs
  LeoAnderson' attrs -> coerce attrs
  LolaHayes' attrs -> coerce attrs
  LukeRobinson' attrs -> coerce attrs
  MandyThompson' attrs -> coerce attrs
  MarieLambeau' attrs -> coerce attrs
  MarkHarrigan' attrs -> coerce attrs
  MinhThiPhan' attrs -> coerce attrs
  NathanielCho' attrs -> coerce attrs
  NormanWithers' attrs -> coerce attrs
  PatriceHathaway' attrs -> coerce attrs
  PrestonFairmont' attrs -> coerce attrs
  RexMurphy' attrs -> coerce attrs
  RitaYoung' attrs -> coerce attrs
  RolandBanks' attrs -> coerce attrs
  SefinaRousseau' attrs -> coerce attrs
  SilasMarsh' attrs -> coerce attrs
  SisterMary' attrs -> coerce attrs
  SkidsOToole' attrs -> coerce attrs
  StellaClark' attrs -> coerce attrs
  TommyMuldoon' attrs -> coerce attrs
  TonyMorgan' attrs -> coerce attrs
  UrsulaDowns' attrs -> coerce attrs
  WendyAdams' attrs -> coerce attrs
  WilliamYorick' attrs -> coerce attrs
  WinifredHabbamock' attrs -> coerce attrs
  ZoeySamaras' attrs -> coerce attrs

data Investigator
  = AgnesBaker' AgnesBaker
  | AkachiOnyele' AkachiOnyele
  | AmandaSharpe' AmandaSharpe
  | AshcanPete' AshcanPete
  | CalvinWright' CalvinWright
  | CarolynFern' CarolynFern
  | DaisyWalker' DaisyWalker
  | DexterDrake' DexterDrake
  | DianaStanley' DianaStanley
  | FatherMateo' FatherMateo
  | FinnEdwards' FinnEdwards
  | HarveyWalters' HarveyWalters
  | JacquelineFine' JacquelineFine
  | JennyBarnes' JennyBarnes
  | JimCulver' JimCulver
  | JoeDiamond' JoeDiamond
  | LeoAnderson' LeoAnderson
  | LolaHayes' LolaHayes
  | LukeRobinson' LukeRobinson
  | MandyThompson' MandyThompson
  | MarieLambeau' MarieLambeau
  | MarkHarrigan' MarkHarrigan
  | MinhThiPhan' MinhThiPhan
  | NathanielCho' NathanielCho
  | NormanWithers' NormanWithers
  | PatriceHathaway' PatriceHathaway
  | PrestonFairmont' PrestonFairmont
  | RexMurphy' RexMurphy
  | RitaYoung' RitaYoung
  | RolandBanks' RolandBanks
  | SefinaRousseau' SefinaRousseau
  | SilasMarsh' SilasMarsh
  | SisterMary' SisterMary
  | SkidsOToole' SkidsOToole
  | StellaClark' StellaClark
  | TommyMuldoon' TommyMuldoon
  | TonyMorgan' TonyMorgan
  | UrsulaDowns' UrsulaDowns
  | WendyAdams' WendyAdams
  | WilliamYorick' WilliamYorick
  | WinifredHabbamock' WinifredHabbamock
  | ZoeySamaras' ZoeySamaras
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (InvestigatorRunner env) => RunMessage env Investigator where
  runMessage msg = \case
    AgnesBaker' x -> AgnesBaker' <$> runMessage msg x
    AkachiOnyele' x -> AkachiOnyele' <$> runMessage msg x
    AmandaSharpe' x -> AmandaSharpe' <$> runMessage msg x
    AshcanPete' x -> AshcanPete' <$> runMessage msg x
    CalvinWright' x -> CalvinWright' <$> runMessage msg x
    CarolynFern' x -> CarolynFern' <$> runMessage msg x
    DaisyWalker' x -> DaisyWalker' <$> runMessage msg x
    DexterDrake' x -> DexterDrake' <$> runMessage msg x
    DianaStanley' x -> DianaStanley' <$> runMessage msg x
    FatherMateo' x -> FatherMateo' <$> runMessage msg x
    FinnEdwards' x -> FinnEdwards' <$> runMessage msg x
    HarveyWalters' x -> HarveyWalters' <$> runMessage msg x
    JacquelineFine' x -> JacquelineFine' <$> runMessage msg x
    JennyBarnes' x -> JennyBarnes' <$> runMessage msg x
    JimCulver' x -> JimCulver' <$> runMessage msg x
    JoeDiamond' x -> JoeDiamond' <$> runMessage msg x
    LeoAnderson' x -> LeoAnderson' <$> runMessage msg x
    LolaHayes' x -> LolaHayes' <$> runMessage msg x
    LukeRobinson' x -> LukeRobinson' <$> runMessage msg x
    MandyThompson' x -> MandyThompson' <$> runMessage msg x
    MarieLambeau' x -> MarieLambeau' <$> runMessage msg x
    MarkHarrigan' x -> MarkHarrigan' <$> runMessage msg x
    MinhThiPhan' x -> MinhThiPhan' <$> runMessage msg x
    NathanielCho' x -> NathanielCho' <$> runMessage msg x
    NormanWithers' x -> NormanWithers' <$> runMessage msg x
    PatriceHathaway' x -> PatriceHathaway' <$> runMessage msg x
    PrestonFairmont' x -> PrestonFairmont' <$> runMessage msg x
    RexMurphy' x -> RexMurphy' <$> runMessage msg x
    RitaYoung' x -> RitaYoung' <$> runMessage msg x
    RolandBanks' x -> RolandBanks' <$> runMessage msg x
    SefinaRousseau' x -> SefinaRousseau' <$> runMessage msg x
    SilasMarsh' x -> SilasMarsh' <$> runMessage msg x
    SisterMary' x -> SisterMary' <$> runMessage msg x
    SkidsOToole' x -> SkidsOToole' <$> runMessage msg x
    StellaClark' x -> StellaClark' <$> runMessage msg x
    TommyMuldoon' x -> TommyMuldoon' <$> runMessage msg x
    TonyMorgan' x -> TonyMorgan' <$> runMessage msg x
    UrsulaDowns' x -> UrsulaDowns' <$> runMessage msg x
    WendyAdams' x -> WendyAdams' <$> runMessage msg x
    WilliamYorick' x -> WilliamYorick' <$> runMessage msg x
    WinifredHabbamock' x -> WinifredHabbamock' <$> runMessage msg x
    ZoeySamaras' x -> ZoeySamaras' <$> runMessage msg x

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromJustNote ("Unkown investigator: " <> show iid)
    $ HashMap.lookup iid allInvestigators

instance HasName Investigator where
  getName = investigatorName . investigatorAttrs

instance HasCard () Investigator where
  getCard _ cardId =
    fromJustNote "player does not have this card"
      . find ((== cardId) . getCardId)
      . investigatorHand
      . investigatorAttrs

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance HasInvestigatorStats Stats () Investigator where
  getStats _ i = Stats
    { health = investigatorHealth - investigatorHealthDamage
    , sanity = investigatorSanity - investigatorSanityDamage
    , willpower = skillValueFor SkillWillpower Nothing [] a
    , intellect = skillValueFor SkillIntellect Nothing [] a
    , combat = skillValueFor SkillCombat Nothing [] a
    , agility = skillValueFor SkillAgility Nothing [] a
    }
    where a@Attrs {..} = investigatorAttrs i

instance HasSet EnemyId () Investigator where
  getSet _ = investigatorEngagedEnemies . investigatorAttrs

instance HasCount EnemyCount () Investigator where
  getCount _ = EnemyCount . HashSet.size . getSet @EnemyId ()

instance HasCount ResourceCount () Investigator where
  getCount _ = ResourceCount . investigatorResources . investigatorAttrs

instance HasCount ClueCount () Investigator where
  getCount _ = ClueCount . investigatorClues . investigatorAttrs

instance HasSet AssetId () Investigator where
  getSet _ = investigatorAssets . investigatorAttrs

instance HasLocation Investigator where
  locationOf = investigatorLocation . investigatorAttrs


instance HasSkill Investigator where
  getSkill skillType = skillValueFor skillType Nothing [] . investigatorAttrs

class GetInvestigatorId a where
  getInvestigatorId :: a -> InvestigatorId

instance GetInvestigatorId Investigator where
  getInvestigatorId = investigatorId . investigatorAttrs

isPrey
  :: (HasSet Int SkillType env, HasSet RemainingHealth () env)
  => Prey
  -> env
  -> Investigator
  -> Bool
isPrey AnyPrey _ _ = True
isPrey (HighestSkill skillType) env i =
  fromMaybe 0 (maximumMay . HashSet.toList $ getSet skillType env)
    == skillValueFor skillType Nothing [] (investigatorAttrs i)
isPrey LowestHealth env i =
  fromMaybe
      100
      (minimumMay . map unRemainingHealth . HashSet.toList $ getSet () env)
    == remainingHealth i
isPrey (Bearer bid) _ i =
  unBearerId bid == unInvestigatorId (investigatorId $ investigatorAttrs i)
isPrey SetToBearer _ _ = error "The bearer was not correctly set"

handOf :: Investigator -> [Card]
handOf = view hand . investigatorAttrs

deckOf :: Investigator -> Deck PlayerCard
deckOf = view deck . investigatorAttrs

availableSkillsFor :: Investigator -> SkillType -> [SkillType]
availableSkillsFor i s = possibleSkillTypeChoices s (investigatorAttrs i)

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurn . investigatorAttrs

hasResigned :: Investigator -> Bool
hasResigned = view resigned . investigatorAttrs

isDefeated :: Investigator -> Bool
isDefeated = view defeated . investigatorAttrs

hasClues :: Investigator -> Bool
hasClues i = investigatorAttrs i ^. clues > 0

remainingHealth :: Investigator -> Int
remainingHealth i = investigatorHealth attrs - investigatorHealthDamage attrs
  where attrs = investigatorAttrs i
