{-# LANGUAGE TemplateHaskell #-}

module Arkham.Cost (
  module Arkham.Cost,
  module X,
) where

import Arkham.Prelude

import Arkham.Classes.GameLogger
import Arkham.Cost.Status as X
import Arkham.Zone as X

import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.Calculation
import Arkham.Campaigns.TheForgottenAge.Supply
import {-# SOURCE #-} Arkham.Card
import Arkham.ChaosToken (ChaosToken)
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.Cost.FieldCost
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.SkillType
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Control.Lens (Plated (..), Prism', cosmos, prism', sumOf, toListOf)
import Data.Aeson.TH
import Data.Data.Lens (uniplate)
import Data.Text qualified as T
import GHC.Records

totalActionCost :: Cost -> Int
totalActionCost (ActionCost n) = n
totalActionCost (Costs xs) = sum $ map totalActionCost xs
totalActionCost _ = 0

totalResourceCost :: Cost -> Int
totalResourceCost (ResourceCost n) = n
totalResourceCost (Costs xs) = sum $ map totalResourceCost xs
totalResourceCost _ = 0

totalResourcePayment :: Payment -> Int
totalResourcePayment = sumOf (cosmos . _ResourcePayment)

totalUsesPayment :: Payment -> Int
totalUsesPayment = sumOf (cosmos . _UsesPayment)

totalInvestigatorDamagePayment :: Payment -> Int
totalInvestigatorDamagePayment = sumOf (cosmos . _InvestigatorDamagePayment)

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

discardPayments :: Payment -> [(Zone, Card)]
discardPayments = concat . toListOf (cosmos . _DiscardPayment)

instance HasField "discards" Payment [(Zone, Card)] where
  getField = discardPayments

instance HasField "resources" Payment Int where
  getField = totalResourcePayment

instance HasField "investigatorDamage" Payment Int where
  getField = totalInvestigatorDamagePayment

data Payment
  = ActionPayment Int
  | AdditionalActionPayment
  | CluePayment InvestigatorId Int
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
  | SealChaosTokenPayment ChaosToken
  | ReleaseChaosTokenPayment ChaosToken
  | ReturnChaosTokenToPoolPayment ChaosToken
  | ReturnToHandPayment Card
  | NoPayment
  | SupplyPayment Supply
  deriving stock (Show, Eq, Ord, Data)

instance Plated Payment where
  plate = uniplate

_DiscardPayment :: Prism' Payment [(Zone, Card)]
_DiscardPayment = prism' DiscardPayment $ \case
  DiscardPayment x -> Just x
  _ -> Nothing

_ResourcePayment :: Prism' Payment Int
_ResourcePayment = prism' ResourcePayment $ \case
  ResourcePayment x -> Just x
  _ -> Nothing

_UsesPayment :: Prism' Payment Int
_UsesPayment = prism' UsesPayment $ \case
  UsesPayment x -> Just x
  _ -> Nothing

_InvestigatorDamagePayment :: Prism' Payment Int
_InvestigatorDamagePayment = prism' InvestigatorDamagePayment $ \case
  InvestigatorDamagePayment x -> Just x
  _ -> Nothing

data Cost
  = ActionCost Int
  | IncreaseCostOfThis CardId Int
  | AdditionalActionsCost
  | AssetClueCost Text AssetMatcher GameValue
  | ClueCost GameValue
  | ClueCostX
  | GroupClueCost GameValue LocationMatcher
  | GroupClueCostRange (Int, Int) LocationMatcher
  | PlaceClueOnLocationCost Int
  | ExhaustCost Target
  | DiscardAssetCost AssetMatcher
  | ExhaustAssetCost AssetMatcher
  | ExhaustXAssetCost AssetMatcher
  | RemoveCost Target
  | RevealCost CardId
  | Costs [Cost]
  | OrCost [Cost]
  | DamageCost Source Target Int
  | DirectDamageCost Source InvestigatorMatcher Int
  | InvestigatorDamageCost Source InvestigatorMatcher DamageStrategy Int
  | DiscardTopOfDeckCost Int
  | DiscardCost Zone Target
  | DiscardCardCost Card
  | DiscardRandomCardCost
  | DiscardFromCost Int CostZone CardMatcher
  | DiscardDrawnCardCost
  | DiscardHandCost
  | DoomCost Source Target Int
  | EnemyDoomCost Int EnemyMatcher
  | EnemyAttackCost EnemyId
  | ExileCost Target
  | HandDiscardCost Int CardMatcher
  | HandDiscardAnyNumberCost CardMatcher
  | ReturnMatchingAssetToHandCost AssetMatcher
  | ReturnAssetToHandCost AssetId
  | SkillTestCost Source SkillType GameCalculation
  | SkillIconCost Int (Set SkillIcon)
  | SameSkillIconCost Int
  | DiscardCombinedCost Int
  | ShuffleDiscardCost Int CardMatcher
  | HorrorCost Source Target Int
  | HorrorCostX Source -- for The Black Book
  | Free
  | ScenarioResourceCost Int
  | ResourceCost Int
  | FieldResourceCost FieldCost
  | MaybeFieldResourceCost MaybeFieldCost
  | UseCost AssetMatcher UseType Int
  | EventUseCost EventMatcher UseType Int
  | DynamicUseCost AssetMatcher UseType DynamicUseCostValue
  | UseCostUpTo AssetMatcher UseType Int Int -- (e.g. Spend 1-5 ammo, see M1918 BAR)
  | CostWhenEnemy EnemyMatcher Cost
  | CostIfEnemy EnemyMatcher Cost Cost
  | UpTo Int Cost
  | SealCost ChaosTokenMatcher
  | SealMultiCost Int ChaosTokenMatcher
  | AddCurseTokenCost Int
  | AddCurseTokensEqualToShroudCost
  | AddCurseTokensEqualToSkillTestDifficulty
  | ReleaseChaosTokenCost ChaosToken
  | ReleaseChaosTokensCost Int ChaosTokenMatcher
  | SealChaosTokenCost ChaosToken -- internal to track sealed token
  | ReturnChaosTokensToPoolCost Int ChaosTokenMatcher
  | ReturnChaosTokenToPoolCost ChaosToken
  | SupplyCost LocationMatcher Supply
  | ResolveEachHauntedAbility LocationId -- the circle undone, see TrappedSpirits
  | ShuffleBondedCost Int CardCode
  | ShuffleIntoDeckCost Target
  | ShuffleAttachedCardIntoDeckCost Target CardMatcher
  | OptionalCost Cost
  | UnpayableCost
  | AsIfAtLocationCost LocationId Cost
  | NonBlankedCost Cost
  | DrawEncounterCardsCost Int
  | GloriaCost -- lol, not going to attempt to make this generic
  | ArchiveOfConduitsUnidentifiedCost -- this either
  deriving stock (Show, Eq, Ord, Data)

assetUseCost :: (Entity a, EntityId a ~ AssetId) => a -> UseType -> Int -> Cost
assetUseCost a uType n = UseCost (AssetWithId $ toId a) uType n

exhaust :: Targetable a => a -> Cost
exhaust = ExhaustCost . toTarget

discardCost :: Targetable a => a -> Cost
discardCost = DiscardCost FromPlay . toTarget

exileCost :: Targetable a => a -> Cost
exileCost = ExileCost . toTarget

removeCost :: Targetable a => a -> Cost
removeCost = RemoveCost . toTarget

data DynamicUseCostValue = DrawnCardsValue
  deriving stock (Show, Eq, Ord, Data)

displayCostType :: Cost -> Text
displayCostType = \case
  ExhaustXAssetCost _ -> "Exhaust X copies"
  EnemyAttackCost _ -> "The chosen enemy makes an attack against you"
  UnpayableCost -> "Unpayable"
  OptionalCost c -> "Optional: " <> displayCostType c
  DrawEncounterCardsCost n -> "Draw " <> pluralize n "Encounter Card"
  NonBlankedCost c -> displayCostType c
  GloriaCost ->
    "Discard an encounter card that shares a Trait with that encounter card from beneath Gloria Goldberg"
  ArchiveOfConduitsUnidentifiedCost ->
    "Place 1 resource on 4 different locations, as leylines."
  CostWhenEnemy _ c -> displayCostType c
  CostIfEnemy _ _ c -> displayCostType c
  AsIfAtLocationCost _ c -> displayCostType c
  ShuffleAttachedCardIntoDeckCost _ _ -> "Shuffle attached card into deck"
  AddCurseTokenCost n -> "Add " <> tshow n <> " {curse} " <> pluralize n "token" <> "to the chaos bag"
  AddCurseTokensEqualToShroudCost -> "Add {curse} tokens to the chaos bag equal to your location's shroud value"
  AddCurseTokensEqualToSkillTestDifficulty -> "Add {curse} tokens to this test's difficulty"
  ShuffleIntoDeckCost _ -> "Shuffle into deck"
  ShuffleBondedCost n cCode -> case lookupCardDef cCode of
    Just def ->
      "You must search your bonded cards for "
        <> irregular n "copy" "copies"
        <> " of "
        <> toTitle def
        <> " into your deck"
    Nothing -> error "impossible"
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
  AssetClueCost lbl _ gv -> case gv of
    Static n -> pluralize n "Clue" <> " from " <> lbl
    PerPlayer n -> pluralize n "Clue" <> " per Player from " <> lbl
    StaticWithPerPlayer n m ->
      tshow n <> " + " <> tshow m <> " Clues per Player from " <> lbl
    ByPlayerCount a b c d ->
      tshow a
        <> ", "
        <> tshow b
        <> ", "
        <> tshow c
        <> ", or "
        <> tshow d
        <> " Clues for 1, 2, 3, or 4 players from "
        <> lbl
  ClueCost gv -> case gv of
    Static n -> pluralize n "Clue"
    PerPlayer n -> pluralize n "Clue" <> " per Player"
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
  ClueCostX -> "Spend X Clues"
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
  RevealCost _ -> "Reveal this card"
  Costs cs -> T.intercalate ", " $ map displayCostType cs
  OrCost cs -> T.intercalate " or " $ map displayCostType cs
  DamageCost _ _ n -> tshow n <> " Damage"
  DirectDamageCost _ _ n -> tshow n <> " Direct Damage"
  InvestigatorDamageCost _ _ _ n -> tshow n <> " Damage"
  DiscardCost zone _ -> "Discard from " <> zoneLabel zone
  DiscardCardCost _ -> "Discard Card"
  DiscardRandomCardCost -> "Discard Random Card"
  DiscardFromCost n _ _ -> "Discard " <> tshow n
  DiscardDrawnCardCost -> "Discard Drawn Card"
  DoomCost _ _ n -> pluralize n "Doom"
  EnemyDoomCost n _ -> "Place " <> pluralize n "Doom" <> " on a matching enemy"
  ExileCost _ -> "Exile"
  HandDiscardCost n _ -> "Discard " <> tshow n <> " from Hand"
  HandDiscardAnyNumberCost _ -> "Discard any number of cards from you hand"
  ReturnMatchingAssetToHandCost {} -> "Return matching asset to hand"
  ReturnAssetToHandCost {} -> "Return asset to hand"
  SkillIconCost n _ -> tshow n <> " Matching Icons"
  SkillTestCost _ sType n -> "Test {" <> tshow sType <> "}(" <> tshow n <> ")"
  SameSkillIconCost n -> tshow n <> " instances of the same skill icon"
  HorrorCost _ _ n -> tshow n <> " Horror"
  HorrorCostX _ -> "Take X Horror"
  Free -> "Free"
  ResourceCost n -> pluralize n "Resource"
  ScenarioResourceCost n -> pluralize n "Resource from the scenario reference"
  EventUseCost _ b c -> displayCostType (UseCost AnyAsset b c)
  UseCost _ uType n -> case uType of
    Clue -> error "Not a use"
    Damage -> error "Not a use"
    Doom -> error "Not a use"
    Horror -> error "Not a use"
    AlarmLevel -> pluralize n "Alarm Level"
    LostSoul -> pluralize n "Lost Soul"
    Corruption -> tshow n <> " Corruption"
    Depth -> tshow n <> " Depth"
    Aether -> tshow n <> " Aether"
    Ammo -> tshow n <> " Ammo"
    Supply -> if n == 1 then "1 Supply" else tshow n <> " Supplies"
    Secret -> pluralize n "Secret"
    Charge -> pluralize n "Charge"
    Offering -> pluralize n "Offering"
    Try -> if n == 1 then "1 Try" else tshow n <> " Tries"
    Bounty -> if n == 1 then "1 Bounty" else tshow n <> " Bounties"
    Whistle -> pluralize n "Whistle"
    Resource -> pluralize n "Resource from the asset"
    Key -> pluralize n "Key"
    Lock -> pluralize n "Lock"
    Evidence -> tshow n <> " Evidence"
    Leyline -> pluralize n "Leyline"
    Durability -> tshow n <> " Durability"
  DynamicUseCost _ uType _ -> case uType of
    AlarmLevel -> "X Alarm Levels"
    Depth -> "X Depth"
    LostSoul -> "X LostSouls"
    Clue -> error "Not a use"
    Damage -> error "Not a use"
    Doom -> error "Not a use"
    Horror -> error "Not a use"
    Corruption -> "X Corruptions"
    Aether -> "X Aether"
    Ammo -> "X Ammo"
    Supply -> "X Supplies"
    Secret -> "X Secrets"
    Charge -> "X Charges"
    Offering -> "X Offerings"
    Durability -> "X Durability"
    Try -> "X Tries"
    Bounty -> "X Bounties"
    Whistle -> "X Whistles"
    Resource -> "X Resources"
    Key -> "X Keys"
    Lock -> "X Locks"
    Evidence -> "X Evidence"
    Leyline -> "X Leylines"
  UseCostUpTo _ uType n m -> case uType of
    AlarmLevel -> tshow n <> "-" <> tshow m <> " Alarm Levels"
    Depth -> tshow n <> "-" <> tshow m <> " Depth"
    LostSoul -> tshow n <> "-" <> tshow m <> " Lost Souls"
    Clue -> error "Not a use"
    Doom -> error "Not a use"
    Horror -> error "Not a use"
    Damage -> error "Not a use"
    Aether -> tshow n <> "-" <> tshow m <> " Aether"
    Corruption -> tshow n <> "-" <> tshow m <> " Corruption"
    Ammo -> tshow n <> "-" <> tshow m <> " Ammo"
    Supply -> tshow n <> "-" <> tshow m <> " Supplies"
    Secret -> tshow n <> "-" <> tshow m <> " Secrets"
    Charge -> tshow n <> "-" <> tshow m <> " Charges"
    Offering -> tshow n <> "-" <> tshow m <> " Offerings"
    Durability -> tshow n <> "-" <> tshow m <> " Durability"
    Try -> tshow n <> "-" <> tshow m <> " Tries"
    Bounty -> tshow n <> "-" <> tshow m <> " Bounties"
    Whistle -> tshow n <> "-" <> tshow m <> " Whistles"
    Resource -> tshow n <> "-" <> tshow m <> " Resources"
    Key -> tshow n <> "-" <> tshow m <> " Keys"
    Lock -> tshow n <> "-" <> tshow m <> " Locks"
    Evidence -> tshow n <> "-" <> tshow m <> " Evidence"
    Leyline -> tshow n <> "-" <> tshow m <> " Leylines"
  UpTo n c -> displayCostType c <> " up to " <> pluralize n "time"
  SealCost _ -> "Seal token"
  SealMultiCost n _ -> "Seal " <> tshow n <> " matching tokens"
  SealChaosTokenCost _ -> "Seal token"
  ReleaseChaosTokenCost _ -> "Release a chaos token sealed here"
  ReleaseChaosTokensCost 1 _ -> "Release a chaos token sealed here"
  ReleaseChaosTokensCost _ _ -> "Release chaos tokens sealed here"
  ReturnChaosTokensToPoolCost n (IncludeSealed _) ->
    "Search the chaos bag and/or cards in play for a total of "
      <> tshow n
      <> " matching tokens and return them to the token pool"
  ReturnChaosTokensToPoolCost n _ ->
    "Search the chaos bag for a total of "
      <> tshow n
      <> " matching tokens and return them to the token pool"
  ReturnChaosTokenToPoolCost token ->
    "Return " <> format token <> " to the token pool"
  FieldResourceCost {} -> "X"
  MaybeFieldResourceCost {} -> "X"
  SupplyCost _ supply ->
    "An investigator crosses off " <> tshow supply <> " from their supplies"
  IncreaseCostOfThis _ n -> "Increase its cost by " <> tshow n

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
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup CostZone where
  CostZones xs <> CostZones ys = CostZones (xs <> ys)
  CostZones xs <> y = CostZones (xs <> [y])
  x <> CostZones ys = CostZones (x : ys)
  x <> y = CostZones [x, y]

$(deriveJSON defaultOptions ''CostZone)
$(deriveJSON defaultOptions ''DynamicUseCostValue)
$(deriveJSON defaultOptions ''Cost)
$(deriveJSON defaultOptions ''Payment)
