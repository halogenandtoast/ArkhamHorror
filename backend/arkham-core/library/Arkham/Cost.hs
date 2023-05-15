{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost (
  module Arkham.Cost,
  module X,
) where

import Arkham.Prelude

import Arkham.Cost.Status as X
import Arkham.Zone as X

import Arkham.Asset.Uses
import Arkham.Campaigns.TheForgottenAge.Supply
import {-# SOURCE #-} Arkham.Card
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Cost.FieldCost
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Token (Token)
import Data.Aeson.TH
import Data.Text qualified as T

totalActionCost :: Cost -> Int
totalActionCost (ActionCost n) = n
totalActionCost (Costs xs) = sum $ map totalActionCost xs
totalActionCost _ = 0

totalResourceCost :: Cost -> Int
totalResourceCost (ResourceCost n) = n
totalResourceCost (Costs xs) = sum $ map totalResourceCost xs
totalResourceCost _ = 0

totalResourcePayment :: Payment -> Int
totalResourcePayment (ResourcePayment n) = n
totalResourcePayment (Payments xs) = sum $ map totalResourcePayment xs
totalResourcePayment _ = 0

decreaseActionCost :: Cost -> Int -> Cost
decreaseActionCost (ActionCost x) y = ActionCost $ max 0 (x - y)
decreaseActionCost (Costs (a : as)) y = case a of
  ActionCost x | x >= y -> Costs (ActionCost (x - y) : as)
  ActionCost x ->
    ActionCost (max 0 (x - y)) <> decreaseActionCost (Costs as) (y - x)
  _ -> a <> decreaseActionCost (Costs as) y
decreaseActionCost other _ = other

increaseActionCost :: Cost -> Int -> Cost
increaseActionCost (ActionCost x) y = ActionCost $ max 0 (x + y)
increaseActionCost (Costs (a : as)) y = case a of
  ActionCost x -> Costs (ActionCost (x + y) : as)
  _ -> a <> increaseActionCost (Costs as) y
increaseActionCost other _ = other

increaseResourceCost :: Cost -> Int -> Cost
increaseResourceCost (ResourceCost x) y = ResourceCost $ max 0 (x + y)
increaseResourceCost (Costs (a : as)) y = case a of
  ResourceCost x -> Costs (ResourceCost (x + y) : as)
  _ -> a <> increaseResourceCost (Costs as) y
increaseResourceCost other _ = other

data Payment
  = ActionPayment Int
  | AdditionalActionPayment
  | CluePayment Int
  | DoomPayment Int
  | ResourcePayment Int
  | CardPayment Card
  | DiscardPayment [(Zone, Card)]
  | DiscardCardPayment [Card]
  | ExhaustPayment [Target]
  | RemovePayment [Target]
  | ExilePayment [Target]
  | UsesPayment Int
  | HorrorPayment Int
  | DamagePayment Int
  | DirectDamagePayment Int
  | InvestigatorDamagePayment Int
  | SkillIconPayment [SkillIcon]
  | Payments [Payment]
  | SealTokenPayment Token
  | ReleaseTokenPayment Token
  | ReturnToHandPayment Card
  | NoPayment
  | SupplyPayment Supply
  deriving stock (Show, Eq, Ord)

data Cost
  = ActionCost Int
  | IncreaseCostOfThis CardId Int
  | AdditionalActionsCost
  | ClueCost Int
  | PerPlayerClueCost Int
  | GroupClueCost GameValue LocationMatcher
  | GroupClueCostRange (Int, Int) LocationMatcher
  | PlaceClueOnLocationCost Int
  | ExhaustCost Target
  | DiscardAssetCost AssetMatcher
  | ExhaustAssetCost AssetMatcher
  | RemoveCost Target
  | Costs [Cost]
  | OrCost [Cost]
  | DamageCost Source Target Int
  | DirectDamageCost Source InvestigatorMatcher Int
  | InvestigatorDamageCost Source InvestigatorMatcher DamageStrategy Int
  | DiscardTopOfDeckCost Int
  | DiscardCost Zone Target
  | DiscardCardCost Card
  | DiscardFromCost Int CostZone CardMatcher
  | DiscardDrawnCardCost
  | DiscardHandCost
  | DoomCost Source Target Int
  | EnemyDoomCost Int EnemyMatcher
  | ExileCost Target
  | HandDiscardCost Int CardMatcher
  | ReturnMatchingAssetToHandCost AssetMatcher
  | ReturnAssetToHandCost AssetId
  | SkillIconCost Int (Set SkillIcon)
  | DiscardCombinedCost Int
  | ShuffleDiscardCost Int CardMatcher
  | HorrorCost Source Target Int
  | HorrorCostX Source -- for The Black Book
  | Free
  | ResourceCost Int
  | FieldResourceCost FieldCost
  | UseCost AssetMatcher UseType Int
  | DynamicUseCost AssetMatcher UseType DynamicUseCostValue
  | UseCostUpTo AssetMatcher UseType Int Int -- (e.g. Spend 1-5 ammo, see M1918 BAR)
  | UpTo Int Cost
  | SealCost TokenMatcher
  | ReleaseTokenCost Token
  | ReleaseTokensCost Int
  | SealTokenCost Token -- internal to track sealed token
  | SupplyCost LocationMatcher Supply
  | ResolveEachHauntedAbility LocationId -- the circle undone, see TrappedSpirits
  deriving stock (Show, Eq, Ord)

data DynamicUseCostValue = DrawnCardsValue
  deriving stock (Show, Eq, Ord)

displayCostType :: Cost -> Text
displayCostType = \case
  ResolveEachHauntedAbility _ -> "Resolve each haunted ability on this location"
  ActionCost n -> pluralize n "Action"
  DiscardTopOfDeckCost n -> pluralize n "Card" <> " from the top of your deck"
  DiscardAssetCost _ -> "Discard matching asset"
  DiscardCombinedCost n ->
    "Discard cards with a total combined cost of at least " <> tshow n
  DiscardHandCost -> "Discard your entire hand"
  ShuffleDiscardCost n _ ->
    "Shuffle " <> pluralize n "matching card" <> " into your deck"
  AdditionalActionsCost -> "Additional Action"
  ClueCost n -> pluralize n "Clue"
  PerPlayerClueCost n -> pluralize n "Clue" <> " per player"
  GroupClueCost gv _ -> case gv of
    Static n -> pluralize n "Clue" <> " as a Group"
    PerPlayer n -> pluralize n "Clue" <> " per Player as a Group"
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player"
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players"
  GroupClueCostRange (sVal, eVal) _ ->
    tshow sVal <> "-" <> pluralize eVal "Clue" <> " as a Group"
  PlaceClueOnLocationCost n ->
    "Place " <> pluralize n "Clue" <> " on your location"
  ExhaustCost _ -> "Exhaust"
  ExhaustAssetCost _ -> "Exhaust matching asset"
  RemoveCost _ -> "Remove from play"
  Costs cs -> T.intercalate ", " $ map displayCostType cs
  OrCost cs -> T.intercalate " or " $ map displayCostType cs
  DamageCost _ _ n -> tshow n <> " Damage"
  DirectDamageCost _ _ n -> tshow n <> " Direct Damage"
  InvestigatorDamageCost _ _ _ n -> tshow n <> " Damage"
  DiscardCost zone _ -> "Discard from" <> tshow zone
  DiscardCardCost _ -> "Discard Card"
  DiscardFromCost n _ _ -> "Discard " <> tshow n
  DiscardDrawnCardCost -> "Discard Drawn Card"
  DoomCost _ _ n -> pluralize n "Doom"
  EnemyDoomCost n _ -> "Place " <> pluralize n "Doom" <> " on a matching enemy"
  ExileCost _ -> "Exile"
  HandDiscardCost n _ -> "Discard " <> tshow n <> " from Hand"
  ReturnMatchingAssetToHandCost {} -> "Return matching asset to hand"
  ReturnAssetToHandCost {} -> "Return asset to hand"
  SkillIconCost n _ -> tshow n <> " Matching Icons"
  HorrorCost _ _ n -> tshow n <> " Horror"
  HorrorCostX _ -> "Take X Horror"
  Free -> "Free"
  ResourceCost n -> pluralize n "Resource"
  UseCost _ uType n -> case uType of
    Ammo -> tshow n <> " Ammo"
    Supply -> if n == 1 then "1 Supply" else tshow n <> " Supplies"
    Secret -> pluralize n "Secret"
    Charge -> pluralize n "Charge"
    Try -> if n == 1 then "1 Try" else tshow n <> " Tries"
    Bounty -> if n == 1 then "1 Bounty" else tshow n <> " Bounties"
    Whistle -> pluralize n "Whistle"
    Resource -> pluralize n "Resource from the asset"
    Key -> pluralize n "Key"
    Lock -> pluralize n "Lock"
  DynamicUseCost _ uType _ -> case uType of
    Ammo -> "X Ammo"
    Supply -> "X Supplies"
    Secret -> "X Secrets"
    Charge -> "X Charges"
    Try -> "X Tries"
    Bounty -> "X Bounties"
    Whistle -> "X Whistles"
    Resource -> "X Resources"
    Key -> "X Keys"
    Lock -> "X Locks"
  UseCostUpTo _ uType n m -> case uType of
    Ammo -> tshow n <> "-" <> tshow m <> " Ammo"
    Supply -> tshow n <> "-" <> tshow m <> " Supplies"
    Secret -> tshow n <> "-" <> tshow m <> " Secrets"
    Charge -> tshow n <> "-" <> tshow m <> " Charges"
    Try -> tshow n <> "-" <> tshow m <> " Tries"
    Bounty -> tshow n <> "-" <> tshow m <> " Bounties"
    Whistle -> tshow n <> "-" <> tshow m <> " Whistles"
    Resource -> tshow n <> "-" <> tshow m <> " Resources"
    Key -> tshow n <> "-" <> tshow m <> " Keys"
    Lock -> tshow n <> "-" <> tshow m <> " Locks"
  UpTo n c -> displayCostType c <> " up to " <> pluralize n "time"
  SealCost _ -> "Seal token"
  SealTokenCost _ -> "Seal token"
  ReleaseTokenCost _ -> "Release a chaos token sealed here"
  ReleaseTokensCost 1 -> "Release a chaos token sealed here"
  ReleaseTokensCost _ -> "Release chaos tokens sealed here"
  FieldResourceCost {} -> "X"
  SupplyCost _ supply ->
    "An investigator crosses off " <> tshow supply <> " from their supplies"
  IncreaseCostOfThis _ n -> "Increase its cost by " <> tshow n
 where
  pluralize n a = if n == 1 then "1 " <> a else tshow n <> " " <> a <> "s"

instance Semigroup Cost where
  Free <> a = a
  a <> Free = a
  Costs xs <> Costs ys = Costs (xs <> ys)
  Costs xs <> a = Costs (a : xs)
  a <> Costs xs = Costs (a : xs)
  a <> b = Costs [a, b]

instance Monoid Cost where
  mempty = Free

instance Semigroup Payment where
  NoPayment <> a = a
  a <> NoPayment = a
  Payments xs <> Payments ys = Payments (xs <> ys)
  Payments xs <> a = Payments (a : xs)
  a <> Payments xs = Payments (a : xs)
  a <> b = Payments [a, b]

data CostZone
  = FromHandOf InvestigatorMatcher
  | FromPlayAreaOf InvestigatorMatcher
  | CostZones [CostZone]
  deriving stock (Show, Eq, Ord)

instance Semigroup CostZone where
  CostZones xs <> CostZones ys = CostZones (xs <> ys)
  CostZones xs <> y = CostZones (xs <> [y])
  x <> CostZones ys = CostZones (x : ys)
  x <> y = CostZones [x, y]

$(deriveJSON defaultOptions ''CostZone)
$(deriveJSON defaultOptions ''DynamicUseCostValue)
$(deriveJSON defaultOptions ''Cost)
$(deriveJSON defaultOptions ''Payment)
