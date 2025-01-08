{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Asset where

import Arkham.Asset.Uses
import Arkham.Card.CardCode
import Arkham.Card.Id
import Arkham.ClassSymbol
import Arkham.Damage
import Arkham.Id
import Arkham.Keyword (Keyword (..))
import {-# SOURCE #-} Arkham.Matcher.Ability
import Arkham.Matcher.Base
import Arkham.Matcher.Card
import Arkham.Matcher.ChaosToken
import Arkham.Matcher.Event
import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Matcher.Location
import Arkham.Matcher.Target
import Arkham.Matcher.Value
import {-# SOURCE #-} Arkham.Modifier
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Arkham.SlotType
import {-# SOURCE #-} Arkham.Source
import Arkham.Trait (Trait (..))
import Data.Aeson.TH
import GHC.OverloadedLabels

instance IsMatcher AssetMatcher

pattern AssetCanHaveUses :: UseType -> AssetMatcher
pattern AssetCanHaveUses uType <-
  AssetOneOf [AssetMatches [AssetWithUseType uType, AssetNotAtUseLimit], AssetWithoutUses]
  where
    AssetCanHaveUses uType = AssetOneOf [AssetMatches [AssetWithUseType uType, AssetNotAtUseLimit], AssetWithoutUses]

data AssetMatcher
  = AssetWithTitle Text
  | AssetWithFullTitle Text Text
  | AssetWithSubtitle Text
  | AssetWithId AssetId
  | AssetWithClass ClassSymbol
  | AssetWithTrait Trait
  | AssetWithKeyword Keyword
  | AssetAttachedToAsset AssetMatcher
  | AssetWithAttachedEvent EventMatcher
  | AssetAttachedTo TargetMatcher
  | AssetControlledBy InvestigatorMatcher
  | AssetInPlayAreaOf InvestigatorMatcher
  | AssetOwnedBy InvestigatorMatcher
  | UnownedAsset
  | AssetMatches [AssetMatcher]
  | AssetOneOf [AssetMatcher]
  | AssetAtLocation LocationId
  | AssetNonStory
  | AssetReady
  | AssetExhausted
  | AssetCanLeavePlayByNormalMeans
  | AssetWithModifier ModifierType
  | AssetWithoutModifier ModifierType
  | AssetNotAtUseLimit
  | AssetNotAtUsesX
  | AssetWithUseType UseType
  | AssetWithUses UseType
  | AssetWithoutUses
  | AssetWithUseCount UseType ValueMatcher
  | AssetWithDoom ValueMatcher
  | AssetWithClues ValueMatcher
  | AssetWithTokens ValueMatcher Token
  | AssetWithSpendableUses ValueMatcher UseType
  | AssetWithHighestPrintedCost AssetMatcher
  | AssetWithSealedChaosTokens Int ChaosTokenMatcher
  | AssetWithoutSealedTokens
  | AssetInSlot SlotType
  | AssetInTwoHandSlots
  | AssetInSingleHand
  | AssetIs CardCode
  | AssetWithCardId CardId
  | AssetCardMatch CardMatcher
  | AnyAsset
  | NotAsset AssetMatcher
  | EnemyAsset EnemyId
  | AssetAt LocationMatcher
  | DiscardableAsset
  | AssetWithDamage
  | AssetWithHorror
  | AssetWithHealth
  | AssetWithSanity
  | AssetWithAnyRemainingHealth
  | AssetWithAnyRemainingSanity
  | AssetWithFewestClues AssetMatcher
  | AssetCanBeAssignedDamageBy InvestigatorId
  | AssetCanBeDamagedBySource Source
  | AssetCanBeAssignedHorrorBy InvestigatorId
  | AssetWithCardsUnderneath CardListMatcher
  | ClosestAsset LocationId AssetMatcher
  | NonWeaknessAsset
  | AssetWithMatchingSkillTestIcon
  | UniqueAsset
  | PermanentAsset
  | AssetWithDifferentTitleFromAtLeastOneCardInHand InvestigatorMatcher ExtendedCardMatcher AssetMatcher
  | HealableAsset Source DamageType AssetMatcher
  | AssetWithPlacement Placement
  | AssetWithPerformableAbility AbilityMatcher [ModifierType]
  | AssetWithPerformableAbilityBy InvestigatorMatcher AbilityMatcher [ModifierType]
  | VehicleWithInvestigator InvestigatorMatcher
  deriving stock (Show, Eq, Ord, Data)

asset_ :: AssetMatcher -> AssetMatcher
asset_ = id

instance Not AssetMatcher where
  not_ = NotAsset

instance IsString AssetMatcher where
  fromString = AssetWithTitle . fromString

instance IsLabel "ready" AssetMatcher where
  fromLabel = AssetReady

instance IsLabel "talent" AssetMatcher where
  fromLabel = AssetWithTrait Talent

instance IsLabel "relic" AssetMatcher where
  fromLabel = AssetWithTrait Relic

instance IsLabel "charm" AssetMatcher where
  fromLabel = AssetWithTrait Charm

instance IsLabel "ally" AssetMatcher where
  fromLabel = AssetWithTrait Ally

instance IsLabel "melee" AssetMatcher where
  fromLabel = AssetWithTrait Melee

instance IsLabel "firearm" AssetMatcher where
  fromLabel = AssetWithTrait Firearm

instance IsLabel "weapon" AssetMatcher where
  fromLabel = AssetWithTrait Weapon

instance IsLabel "ranged" AssetMatcher where
  fromLabel = AssetWithTrait Ranged

instance IsLabel "tome" AssetMatcher where
  fromLabel = AssetWithTrait Tome

instance IsLabel "tool" AssetMatcher where
  fromLabel = AssetWithTrait Tool

instance IsLabel "spell" AssetMatcher where
  fromLabel = AssetWithTrait Spell

instance IsLabel "science" AssetMatcher where
  fromLabel = AssetWithTrait Science

instance IsLabel "ritual" AssetMatcher where
  fromLabel = AssetWithTrait Ritual

instance IsLabel "item" AssetMatcher where
  fromLabel = AssetWithTrait Item

instance IsLabel "mystic" AssetMatcher where
  fromLabel = AssetWithClass Mystic

instance IsLabel "hand" AssetMatcher where
  fromLabel = AssetInSlot #hand

instance IsLabel "exhausted" AssetMatcher where
  fromLabel = AssetExhausted

instance Semigroup AssetMatcher where
  AssetWithHighestPrintedCost x <> AssetWithHighestPrintedCost y = AssetWithHighestPrintedCost (x <> y)
  AssetWithHighestPrintedCost x <> y = AssetWithHighestPrintedCost (x <> y)
  x <> AssetWithHighestPrintedCost y = AssetWithHighestPrintedCost (x <> y)
  AssetWithFewestClues x <> AssetWithFewestClues y = AssetWithFewestClues (x <> y)
  AssetWithFewestClues x <> y = AssetWithFewestClues (x <> y)
  x <> AssetWithFewestClues y = AssetWithFewestClues (x <> y)
  ClosestAsset lid x <> ClosestAsset lid' y
    | lid == lid' = ClosestAsset lid (x <> y)
    | otherwise = error "Cannnot combine"
  ClosestAsset lid x <> y = ClosestAsset lid (x <> y)
  x <> ClosestAsset lid y = ClosestAsset lid (x <> y)
  AnyAsset <> x = x
  x <> AnyAsset = x
  AssetMatches xs <> AssetMatches ys = AssetMatches (xs <> ys)
  AssetMatches xs <> x = AssetMatches (x : xs)
  x <> AssetMatches xs = AssetMatches (x : xs)
  x <> y = AssetMatches [x, y]

instance Monoid AssetMatcher where
  mempty = AnyAsset

$(deriveJSON defaultOptions ''AssetMatcher)
