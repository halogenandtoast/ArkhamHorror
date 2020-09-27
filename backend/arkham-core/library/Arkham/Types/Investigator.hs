{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator
  ( isPrey
  , baseInvestigator
  , getEngagedEnemies
  , investigatorAttrs
  , hasEndedTurn
  , hasResigned
  , hasSpendableClues
  , isDefeated
  , remainingHealth
  , remainingSanity
  , actionsRemaining
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
import Arkham.Types.ClassSymbol
import Arkham.Types.EnemyId
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards
import Arkham.Types.Investigator.Runner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson
import Data.Coerce
import Lens.Micro.Extras
import Safe (fromJustNote)

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
  | BaseInvestigator' BaseInvestigator
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

baseInvestigator
  :: InvestigatorId
  -> Text
  -> ClassSymbol
  -> Stats
  -> [Trait]
  -> (Attrs -> Attrs)
  -> Investigator
baseInvestigator a b c d e f =
  BaseInvestigator' . BaseInvestigator . f $ baseAttrs a b c d e

newtype BaseInvestigator = BaseInvestigator Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasActions env investigator BaseInvestigator where
  getActions investigator window (BaseInvestigator attrs) =
    getActions investigator window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env BaseInvestigator where
  runMessage msg (BaseInvestigator attrs) =
    BaseInvestigator <$> runMessage msg attrs

instance (ActionRunner env investigator) => HasActions env investigator Investigator where
  getActions i window investigator | any isBlank (getModifiers investigator) =
    getActions i window (investigatorAttrs investigator)
  getActions i window investigator = defaultGetActions i window investigator

instance (InvestigatorRunner Attrs env) => RunMessage env Investigator where
  runMessage (ResolveToken ElderSign iid skillValue) i
    | iid == getInvestigatorId i && any isBlank (getModifiers i) = i
    <$ runTest iid skillValue 0
  runMessage msg i = defaultRunMessage msg i

instance IsInvestigator Investigator where
  locationOf = locationOf . investigatorAttrs
  canInvestigate location = canInvestigate location . investigatorAttrs
  canMoveTo location = canMoveTo location . investigatorAttrs
  canFight enemy = canFight enemy . investigatorAttrs
  canEngage enemy = canEngage enemy . investigatorAttrs
  canEvade enemy = canEvade enemy . investigatorAttrs
  resourceCount = resourceCount . investigatorAttrs
  clueCount = clueCount . investigatorAttrs
  spendableClueCount = spendableClueCount . investigatorAttrs
  cardCount = cardCount . investigatorAttrs
  discardableCardCount = discardableCardCount . investigatorAttrs
  canDo action = canDo action . investigatorAttrs
  hasActionsRemaining i = hasActionsRemaining (investigatorAttrs i)
  canTakeDirectDamage = canTakeDirectDamage . investigatorAttrs
  discardOf = discardOf . investigatorAttrs

instance HasId InvestigatorId () Investigator where
  getId _ = getId () . investigatorAttrs

instance HasCard () Investigator where
  getCard _ cardId =
    fromJustNote "player does not have this card"
      . find ((== cardId) . getCardId)
      . investigatorHand
      . investigatorAttrs

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance HasModifiers Investigator where
  getModifiers = concat . toList . investigatorModifiers . investigatorAttrs

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

instance HasDamage Investigator where
  getDamage i = (investigatorHealthDamage, investigatorSanityDamage)
    where Attrs {..} = investigatorAttrs i

instance HasTrauma Investigator where
  getTrauma i = (investigatorPhysicalTrauma, investigatorMentalTrauma)
    where Attrs {..} = investigatorAttrs i

instance HasSet EnemyId () Investigator where
  getSet _ = investigatorEngagedEnemies . investigatorAttrs

instance HasCount EnemyCount () Investigator where
  getCount _ = EnemyCount . length . getSet @EnemyId ()

instance HasCount ResourceCount () Investigator where
  getCount _ = ResourceCount . investigatorResources . investigatorAttrs

instance HasCount CardCount () Investigator where
  getCount _ = CardCount . length . investigatorHand . investigatorAttrs

instance HasCount ClueCount () Investigator where
  getCount _ = ClueCount . investigatorClues . investigatorAttrs

instance HasCount SpendableClueCount () Investigator where
  getCount _ i = if canSpendClues (investigatorAttrs i)
    then SpendableClueCount . investigatorClues $ investigatorAttrs i
    else SpendableClueCount 0

instance HasSet AssetId () Investigator where
  getSet _ = investigatorAssets . investigatorAttrs

instance HasSkill Investigator where
  getSkill skillType = skillValueFor skillType Nothing [] . investigatorAttrs

class GetInvestigatorId a where
  getInvestigatorId :: a -> InvestigatorId

instance GetInvestigatorId Investigator where
  getInvestigatorId = investigatorId . investigatorAttrs

allInvestigators :: HashMap InvestigatorId Investigator
allInvestigators = mapFromList $ map
  (toFst $ investigatorId . investigatorAttrs)
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

isBlank :: Modifier -> Bool
isBlank Blank{} = True
isBlank _ = False

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromJustNote ("Unkown investigator: " <> show iid)
    $ lookup iid allInvestigators

getEngagedEnemies :: Investigator -> HashSet EnemyId
getEngagedEnemies = investigatorEngagedEnemies . investigatorAttrs

-- TODO: This does not work for more than 2 players
isPrey
  :: ( HasSet Int SkillType env
     , HasSet RemainingHealth () env
     , HasSet RemainingSanity () env
     , HasSet ClueCount () env
     , HasSet CardCount () env
     )
  => Prey
  -> env
  -> Investigator
  -> Bool
isPrey AnyPrey _ _ = True
isPrey (HighestSkill skillType) env i =
  fromMaybe 0 (maximumMay . toList $ getSet skillType env)
    == skillValueFor skillType Nothing [] (investigatorAttrs i)
isPrey (LowestSkill skillType) env i =
  fromMaybe 100 (minimumMay . toList $ getSet skillType env)
    == skillValueFor skillType Nothing [] (investigatorAttrs i)
isPrey LowestRemainingHealth env i =
  fromMaybe 100 (minimumMay . map unRemainingHealth . toList $ getSet () env)
    == remainingHealth i
isPrey LowestRemainingSanity env i =
  fromMaybe 100 (minimumMay . map unRemainingSanity . toList $ getSet () env)
    == remainingSanity i
isPrey (Bearer bid) _ i =
  unBearerId bid == unInvestigatorId (investigatorId $ investigatorAttrs i)
isPrey MostClues env i =
  fromMaybe 0 (maximumMay . map unClueCount . toList $ getSet () env)
    == unClueCount (getCount () i)
isPrey FewestCards env i =
  fromMaybe 100 (minimumMay . map unCardCount . toList $ getSet () env)
    == unCardCount (getCount () i)
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

hasSpendableClues :: Investigator -> Bool
hasSpendableClues i = spendableClueCount (investigatorAttrs i) > 0

remainingHealth :: Investigator -> Int
remainingHealth i = modifiedHealth attrs - investigatorHealthDamage attrs
  where attrs = investigatorAttrs i

remainingSanity :: Investigator -> Int
remainingSanity i = modifiedSanity attrs - investigatorSanityDamage attrs
  where attrs = investigatorAttrs i

actionsRemaining :: Investigator -> Int
actionsRemaining = investigatorRemainingActions . investigatorAttrs

investigatorAttrs :: Investigator -> Attrs
investigatorAttrs = \case
  AgnesBaker' attrs -> coerce attrs
  AkachiOnyele' attrs -> coerce attrs
  AmandaSharpe' attrs -> coerce attrs
  AshcanPete' attrs -> coerce attrs
  CalvinWright' attrs -> coerce attrs
  CarolynFern' attrs -> coerce attrs
  DaisyWalker' attrs -> coerce attrs
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
  BaseInvestigator' attrs -> coerce attrs
