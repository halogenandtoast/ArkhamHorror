{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Matcher.Investigator where

import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card.CardCode
import Arkham.ClassSymbol
import {-# SOURCE #-} Arkham.Criteria
import Arkham.Damage
import Arkham.Id
import Arkham.Key
import Arkham.Matcher.Asset
import Arkham.Matcher.Base
import Arkham.Matcher.Card
import Arkham.Matcher.ChaosToken
import Arkham.Matcher.Enemy
import Arkham.Matcher.Event
import Arkham.Matcher.Location
import Arkham.Matcher.Skill
import Arkham.Matcher.Treachery
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Arkham.SkillType
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import {-# SOURCE #-} Arkham.Target
import Arkham.Token
import Arkham.Trait (Trait)
import Control.Lens.Plated (Plated)
import Data.Aeson.TH

class IsInvestigatorMatcher a where
  toInvestigatorMatcher :: a -> InvestigatorMatcher

instance IsInvestigatorMatcher InvestigatorId where
  toInvestigatorMatcher = InvestigatorWithId

type Who = InvestigatorMatcher
instance IsMatcher InvestigatorMatcher
instance Be InvestigatorMatcher InvestigatorMatcher where
  be = id

pattern MostClues :: InvestigatorMatcher
pattern MostClues <- MostToken Clue
  where
    MostClues = MostToken Clue

pattern MostHorror :: InvestigatorMatcher
pattern MostHorror <- MostToken Horror
  where
    MostHorror = MostToken Horror

data InvestigatorMatcher
  = InvestigatorAt LocationMatcher
  | InVehicleMatching AssetMatcher
  | IsDriverOf AssetMatcher
  | InvestigatorWithPlacement Placement
  | InvestigatorWhenCriteria Criterion
  | InvestigatorIs CardCode
  | InvestigatorCanBeAssignedDamageBy InvestigatorId
  | InvestigatorCanBeAssignedHorrorBy InvestigatorId
  | InvestigatorCanMoveTo Source LocationMatcher
  | InvestigatorWithToken Token
  | InvestigatorWithSealedChaosToken ChaosTokenMatcher
  | You
  | ThatInvestigator
  | UnengagedInvestigator
  | NoOne
  | NotYou
  | Anyone
  | FewestCardsInHand
  | MostCardsInHand
  | MostDamage
  | LowestRemainingHealth
  | LowestRemainingSanity
  | MostRemainingSanity
  | NearestToEnemy EnemyMatcher
  | NearestToLocation LocationMatcher
  | HasMostMatchingAsset AssetMatcher
  | HasMatchingAsset AssetMatcher
  | OwnsAsset AssetMatcher
  | ControlsAsset AssetMatcher
  | HasMatchingEvent EventMatcher
  | HasMatchingSkill SkillMatcher
  | HasMatchingTreachery TreacheryMatcher
  | InvestigatorWithCommittableCard
  | MostToken Token
  | HasTokens Token ValueMatcher
  | MostKeys
  | InvestigatorWithUnhealedHorror
  | UneliminatedInvestigator
  | ResignedInvestigator
  | DefeatedInvestigator
  | ContributedMatchingIcons ValueMatcher
  | DeckWith CardListMatcher
  | HandWith CardListMatcher
  | DiscardWith CardListMatcher
  | InvestigatorWithTrait Trait
  | InvestigatorWithoutModifier ModifierType
  | InvestigatorWithModifier ModifierType
  | InvestigatorEngagedWith EnemyMatcher
  | InvestigatorWithActionsRemaining ValueMatcher
  | InvestigatorWithClues ValueMatcher
  | InvestigatorWithCluesInPool ValueMatcher
  | InvestigatorWithDamage ValueMatcher
  | InvestigatorHasCardWithDamage
  | InvestigatorHasCardWithHorror
  | InvestigatorWithDoom ValueMatcher
  | InvestigatorWithHorror ValueMatcher
  | InvestigatorWithHealableHorror Source
  | InvestigatorWithRemainingSanity ValueMatcher
  | InvestigatorWithResources ValueMatcher
  | InvestigatorWithSpendableResources ValueMatcher
  | InvestigatorWithId InvestigatorId
  | InvestigatorWithTreacheryInHand TreacheryMatcher
  | InvestigatorWithTitle Text
  | InvestigatorMatches [InvestigatorMatcher]
  | InvestigatorWithLowestSkill SkillType InvestigatorMatcher
  | InvestigatorWithHighestSkill SkillType InvestigatorMatcher
  | AnyInvestigator [InvestigatorMatcher]
  | TurnInvestigator
  | ActiveInvestigator
  | LeadInvestigator
  | NoDamageDealtThisTurn
  | NoSuccessfulExploreThisTurn
  | TopCardOfDeckIs CardMatcher
  | YetToTakeTurn
  | NotInvestigator InvestigatorMatcher
  | InvestigatorThatMovedDuringTurn
  | InvestigatorWithSupply Supply
  | InvestigatorCanDiscoverCluesAtOneOf LocationMatcher -- NOTE: Use matcher above
  | DeckIsEmpty
  | AliveInvestigator
  | IncludeEliminated InvestigatorMatcher
  | HealableInvestigator Source DamageType InvestigatorMatcher
  | InvestigatorWithMostCardsInPlayArea
  | InvestigatorWithClass ClassSymbol
  | InvestigatorWithKey ArkhamKey
  | InvestigatorWithAnyKey
  | InvestigatorWithBondedCard CardMatcher
  | InvestigatorIfThen InvestigatorMatcher InvestigatorMatcher InvestigatorMatcher
  | InvestigatorCanTarget Target
  | InvestigatorWithRecord CampaignLogKey
  | CanBeHuntedBy EnemyId
  | DistanceFromRoundStart ValueMatcher
  | InvestigatorWithMetaKey Text
  | InvestigatorWithFilledSlot SlotType
  | InvestigatorWithAnyFailedSkillTestsThisTurn
  | InvestigatorWithPhysicalTrauma
  | InvestigatorWithMentalTrauma
  deriving stock (Show, Eq, Ord, Data)

instance Plated InvestigatorMatcher

instance Not InvestigatorMatcher where
  not_ = NotInvestigator

instance Semigroup InvestigatorMatcher where
  Anyone <> x = x
  x <> Anyone = x
  InvestigatorMatches xs <> InvestigatorMatches ys =
    InvestigatorMatches $ xs <> ys
  InvestigatorMatches xs <> x = InvestigatorMatches (x : xs)
  x <> InvestigatorMatches xs = InvestigatorMatches (x : xs)
  x <> y = InvestigatorMatches [x, y]

instance Monoid InvestigatorMatcher where
  mempty = Anyone

$(deriveToJSON defaultOptions ''InvestigatorMatcher)

instance FromJSON InvestigatorMatcher where
  parseJSON = withObject "InvestigatorMatcher" \o -> do
    t :: Text <- o .: "tag"
    case t of
      "InvestigatorWithHighestSkill" ->
        (uncurry InvestigatorWithHighestSkill <$> o .: "contents")
          <|> (InvestigatorWithHighestSkill <$> o .: "contents" <*> pure UneliminatedInvestigator)
      "InvestigatorWithLowestSkill" ->
        (uncurry InvestigatorWithLowestSkill <$> o .: "contents")
          <|> (InvestigatorWithLowestSkill <$> o .: "contents" <*> pure UneliminatedInvestigator)
      _ -> $(mkParseJSON defaultOptions ''InvestigatorMatcher) (Object o)
