{-# LANGUAGE ScopedTypeVariables #-}
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
  , actionsRemaining
  , lookupInvestigator
  , availableSkillsFor
  , skillValueOf
  , handOf
  , discardOf
  , deckOf
  , locationOf
  , remainingHealth
  , remainingSanity
  , modifiedStatsOf
  , GetInvestigatorId(..)
  , Investigator
  )
where

import Arkham.Import

import Arkham.Types.Action (Action)
import Arkham.Types.ClassSymbol
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Cards
import Arkham.Types.Investigator.Runner
import Arkham.Types.Prey
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import Data.Coerce

data Investigator
  = AgnesBaker' AgnesBaker
  | AkachiOnyele' AkachiOnyele
  | AmandaSharpe' AmandaSharpe
  | AshcanPete' AshcanPete
  | CalvinWright' CalvinWright
  | CarolynFern' CarolynFern
  | DaisyWalker' DaisyWalker
  | DaisyWalkerParallel' DaisyWalkerParallel
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

deriving anyclass instance InvestigatorRunner env => HasModifiersFor env Investigator

instance Eq Investigator where
  a == b = getInvestigatorId a == getInvestigatorId b

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

instance HasModifiersFor env BaseInvestigator where
  getModifiersFor source target (BaseInvestigator attrs) =
    getModifiersFor source target attrs

instance ActionRunner env => HasActions env BaseInvestigator where
  getActions iid window (BaseInvestigator attrs) = getActions iid window attrs

instance (InvestigatorRunner env) => RunMessage env BaseInvestigator where
  runMessage msg (BaseInvestigator attrs) =
    BaseInvestigator <$> runMessage msg attrs

instance ActionRunner env => HasActions env Investigator where
  getActions iid window investigator = do
    modifiers' <- getModifiers (InvestigatorSource iid) investigator
    if any isBlank modifiers'
      then getActions iid window (investigatorAttrs investigator)
      else defaultGetActions iid window investigator

instance (InvestigatorRunner env) => RunMessage env Investigator where
  runMessage msg@(ResolveToken ElderSign iid) i | iid == getInvestigatorId i =
    do
      modifiers' <- getModifiers (InvestigatorSource iid) i
      if any isBlank modifiers'
        then i <$ runTest iid (TokenValue ElderSign 0)
        else i <$ defaultRunMessage msg i
  runMessage msg i = defaultRunMessage msg i

instance HasId InvestigatorId () Investigator where
  getId _ = getId () . investigatorAttrs

instance HasList DiscardedPlayerCard () Investigator where
  getList _ = map DiscardedPlayerCard . investigatorDiscard . investigatorAttrs

instance HasList HandCard () Investigator where
  getList _ = map HandCard . investigatorHand . investigatorAttrs

instance HasCard () Investigator where
  getCard _ cardId =
    fromJustNote "player does not have this card"
      . find ((== cardId) . getCardId)
      . investigatorHand
      . investigatorAttrs

instance HasCardCode Investigator where
  getCardCode = getCardCode . investigatorAttrs

instance (HasModifiersFor env env) => HasModifiers env Investigator where
  getModifiers source self =
    ask >>= getModifiersFor source (InvestigatorTarget $ getInvestigatorId self)

instance HasDamage Investigator where
  getDamage i = (investigatorHealthDamage, investigatorSanityDamage)
    where Attrs {..} = investigatorAttrs i

instance HasTrauma Investigator where
  getTrauma i = (investigatorPhysicalTrauma, investigatorMentalTrauma)
    where Attrs {..} = investigatorAttrs i

instance HasSet EnemyId () Investigator where
  getSet _ = investigatorEngagedEnemies . investigatorAttrs

instance HasSet TreacheryId () Investigator where
  getSet _ = investigatorTreacheries . investigatorAttrs

instance HasCount ActionRemainingCount (Maybe Action, [Trait]) Investigator where
  getCount (_maction, traits) i =
    let
      tomeActionCount = if Tome `elem` traits
        then fromMaybe 0 (investigatorTomeActions a)
        else 0
    in ActionRemainingCount $ investigatorRemainingActions a + tomeActionCount
    where a = investigatorAttrs i

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
  , DaisyWalkerParallel' daisyWalkerParallel
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

lookupInvestigator :: InvestigatorId -> Investigator
lookupInvestigator iid =
  fromMaybe (lookupPromoInvestigator iid) $ lookup iid allInvestigators

-- | Handle promo investigators
--
-- Some investigators have book versions that are just alternative art
-- with some replacement cards. Since these investigators are functionally
-- the same, we proxy the lookup to their non-promo version.
--
-- Parallel investigators will need to be handled differently since they
-- are not functionally the same.
--
lookupPromoInvestigator :: InvestigatorId -> Investigator
lookupPromoInvestigator "98001" = lookupInvestigator "02003" -- Jenny Barnes
lookupPromoInvestigator "98004" = lookupInvestigator "01001" -- Roland Banks
lookupPromoInvestigator iid = error $ "Unknown investigator: " <> show iid

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

availableSkillsFor :: Investigator -> SkillType -> [SkillType]
availableSkillsFor i s = possibleSkillTypeChoices s (investigatorAttrs i)

skillValueOf :: SkillType -> Investigator -> Int
skillValueOf SkillWillpower = investigatorWillpower . investigatorAttrs
skillValueOf SkillIntellect = investigatorIntellect . investigatorAttrs
skillValueOf SkillCombat = investigatorCombat . investigatorAttrs
skillValueOf SkillAgility = investigatorAgility . investigatorAttrs
skillValueOf SkillWild = error "should not look this up"

handOf :: Investigator -> [Card]
handOf = investigatorHand . investigatorAttrs

discardOf :: Investigator -> [PlayerCard]
discardOf = investigatorDiscard . investigatorAttrs

deckOf :: Investigator -> [PlayerCard]
deckOf = unDeck . investigatorDeck . investigatorAttrs

locationOf :: Investigator -> LocationId
locationOf = investigatorLocation . investigatorAttrs

remainingSanity :: Investigator -> Int
remainingSanity i = modifiedSanity a - investigatorSanityDamage a
  where a = investigatorAttrs i

remainingHealth :: Investigator -> Int
remainingHealth i = modifiedHealth a - investigatorHealthDamage a
  where a = investigatorAttrs i

modifiedStatsOf
  :: (MonadReader env m, HasModifiers env InvestigatorId, MonadIO m)
  => Source
  -> Maybe Action
  -> Investigator
  -> m Stats
modifiedStatsOf source maction i = do
  modifiers' <- getModifiers source (getInvestigatorId i)
  let
    a = investigatorAttrs i
    willpower' = skillValueFor SkillWillpower maction modifiers' a
    intellect' = skillValueFor SkillIntellect maction modifiers' a
    combat' = skillValueFor SkillCombat maction modifiers' a
    agility' = skillValueFor SkillAgility maction modifiers' a
  pure Stats
    { willpower = willpower'
    , intellect = intellect'
    , combat = combat'
    , agility = agility'
    , health = remainingHealth i
    , sanity = remainingSanity i
    }

hasEndedTurn :: Investigator -> Bool
hasEndedTurn = view endedTurn . investigatorAttrs

hasResigned :: Investigator -> Bool
hasResigned = view resigned . investigatorAttrs

isDefeated :: Investigator -> Bool
isDefeated = view defeated . investigatorAttrs

hasSpendableClues :: Investigator -> Bool
hasSpendableClues i = spendableClueCount (investigatorAttrs i) > 0

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
  DaisyWalkerParallel' attrs -> coerce attrs
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
