{- | Expressions over what a custom card's steps have bound.

A @query@ binds a list of ids or cards; on its own that is only ever "how many"
or "the first one". A card that reads "draw 1 card for each different skill icon
among the cards committed" needs to go from that list to a property of each
element and then to a number, which is what this evaluates.

Shape: an expression is a JSON value. Anything that is not one of the operators
below is a literal (with @$bindings@ substituted), so numbers and @"$cards"@
mean themselves. An operator that takes @of@ broadcasts over a list, so @get@ is
both "the property of this one" and "map over these".
-}
module Arkham.Custom.Expr (evalExpr, exprInt) where

import Arkham.Act.Types (Act)
import Arkham.Asset.Types (Asset)
import Arkham.Card
import Arkham.Classes.Entity (EntityId)
import Arkham.Classes.HasGame
import Arkham.Cost (
  addedCurseTokenPayment,
  chosenCardPayment,
  chosenEnemyPayment,
  discardPayment,
  discardedCards,
  exhaustedPayments,
  horrorPaid,
  paymentTargets,
  removedPayments,
  sealChaosTokenPayments,
  totalActionPayment,
  totalCluePayment,
  totalDiscardCardPayments,
  totalInvestigatorDamagePayment,
  totalResourcePayment,
  totalUsesPayment,
 )
import Arkham.Enemy.Types (Enemy)
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Card (getModifiedCardCost)
import Arkham.Helpers.SkillTest (
  getSkillTest,
  getSkillTestAction,
  getSkillTestDifficulty,
  getSkillTestInvestigator,
  getSkillTestMatchingSkillIcons,
 )
import Arkham.Investigator.Types (Investigator)
import Arkham.Location.Types (Location)
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillTest.Base (skillTestCommittedCards, skillTestIconValues, skillTestId)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.List qualified as List
import Data.Text qualified as T

type Env = KeyMap.KeyMap Value

evalExpr :: HasGame m => Env -> Value -> m Value
evalExpr env v0 = case substituteExpr env v0 of
  Object o
    | Just prop <- str =<< KeyMap.lookup "get" o -> withOf o (getProp (kindOf o) prop)
    | Just prop <- str =<< KeyMap.lookup "map" o -> withOf o (getProp (kindOf o) prop)
    | Just p <- KeyMap.lookup "filter" o -> do
        p' <- evalPredicate p
        listOp o (filter (matches p'))
    | Just prop <- str =<< KeyMap.lookup "skillTest" o -> skillTestProp prop
    | Just name <- str =<< KeyMap.lookup "apply" o ->
        applyFn name <$> evalExpr env (fromMaybe Null (KeyMap.lookup "to" o))
    | Just e <- KeyMap.lookup "iconValue" o -> do
        icons <- valueList <$> evalExpr env e
        values <- maybe mempty skillTestIconValues <$> getSkillTest
        let valueOf v = maybe 0 (\i -> findWithDefault 0 i values) (parseMaybe parseJSON v)
        pure $ toJSON (sum (map valueOf icons))
    | Just e <- KeyMap.lookup "unique" o -> unary e (List.nub . valueList)
    | Just e <- KeyMap.lookup "concat" o -> unary e (concatMap valueList . valueList)
    | Just e <- KeyMap.lookup "reverse" o -> unary e (reverse . valueList)
    | Just e <- KeyMap.lookup "count" o -> number e (length . valueList)
    | Just e <- KeyMap.lookup "sum" o -> number e (sum . nums)
    | Just e <- KeyMap.lookup "max" o -> number e (safely List.maximum . nums)
    | Just e <- KeyMap.lookup "min" o -> number e (safely List.minimum . nums)
    | Just e <- KeyMap.lookup "first" o -> unaryV e (fromMaybe Null . headMay . valueList)
    | Just e <- KeyMap.lookup "add" o -> number e (sum . nums)
    | Just e <- KeyMap.lookup "multiply" o -> number e (product . nums)
    | Just e <- KeyMap.lookup "subtract" o -> number e (pairwise (-))
    | Just e <- KeyMap.lookup "divide" o -> number e (pairwise safeDiv)
  v -> pure v
 where
  -- A predicate's operand is an expression too, so @{"in": {"skillTest": ...}}@
  -- compares against what that works out to rather than against the literal.
  evalPredicate = \case
    Object po -> Object <$> traverse (evalExpr env) po
    p -> evalExpr env p
  unary e f = unaryV e (toJSON . f)
  unaryV e f = f <$> evalExpr env e
  number e f = unaryV e (toJSON . f)
  listOp o f = case KeyMap.lookup "of" o of
    Nothing -> pure Null
    Just e -> toJSON . f . valueList <$> evalExpr env e
  withOf o f = case KeyMap.lookup "of" o of
    Nothing -> pure Null
    Just e ->
      evalExpr env e >>= \case
        Array xs -> Array <$> traverse f xs
        x -> f x
  kindOf o = fromMaybe "" (str =<< KeyMap.lookup "kind" o)
  pairwise f xs = case nums xs of
    (a : rest) -> foldl' f a rest
    [] -> 0
  safeDiv a b = if b == 0 then 0 else a `div` b
  safely f xs = if null xs then 0 else f xs

{- | Substitution stops at an operator: @$cards@ inside a @get@ is resolved when
that @get@ evaluates its @of@, and rewriting the operator itself first would
only re-walk what is about to be walked anyway.
-}
substituteExpr :: Env -> Value -> Value
substituteExpr env = \case
  String t | Just name <- T.stripPrefix "$" t -> fromMaybe (String t) (KeyMap.lookup (Key.fromText name) env)
  Object o -> Object (fmap (substituteExpr env) o)
  Array xs -> Array (fmap (substituteExpr env) xs)
  v -> v

-- | An expression whose value is wanted as a count.
exprInt :: HasGame m => Env -> Value -> m Int
exprInt env = fmap toInt . evalExpr env

valueList :: Value -> [Value]
valueList = \case
  Array xs -> toList xs
  Null -> []
  v -> [v]

nums :: Value -> [Int]
nums = map toInt . valueList

toInt :: Value -> Int
toInt = \case
  v@(Number _) -> fromMaybe 0 (parseMaybe parseJSON v)
  Array xs -> length xs
  Bool True -> 1
  _ -> 0

str :: Value -> Maybe Text
str = \case
  String t -> Just t
  _ -> Nothing

-- | What @filter@ keeps. A bare value means equality.
matches :: Value -> Value -> Bool
matches p v = case p of
  Object o
    | Just x <- KeyMap.lookup "eq" o -> v == x
    | Just x <- KeyMap.lookup "ne" o -> v /= x
    | Just x <- KeyMap.lookup "in" o -> v `elem` valueList x
    | Just x <- KeyMap.lookup "notIn" o -> v `notElem` valueList x
    | Just x <- KeyMap.lookup "gt" o -> toInt v > toInt x
    | Just x <- KeyMap.lookup "lt" o -> toInt v < toInt x
    | Just x <- KeyMap.lookup "gte" o -> toInt v >= toInt x
    | Just x <- KeyMap.lookup "lte" o -> toInt v <= toInt x
  _ -> v == p

{- | The named transformations a value can be put through.

The engine already knows how to read a 'Payment' apart -- what it discarded,
what it exhausted, how many resources it cost -- and those readings are what a
card means when it says "the card you discarded". Exposing them by name beats
binding one of them eagerly: a cost that took no cards should offer no cards,
and a card that wants what was exhausted should not need a new binding invented
for it.
-}
applyFn :: Text -> Value -> Value
applyFn name v = case parseMaybe parseJSON v of
  Nothing -> Null
  Just payment -> case name of
    "paidCards" -> toJSON (discardedCards payment)
    "discardedCard" -> toJSON (discardPayment payment)
    "chosenCard" -> toJSON (chosenCardPayment payment)
    "chosenEnemy" -> toJSON (chosenEnemyPayment payment)
    "exhausted" -> toJSON (exhaustedPayments payment)
    "removed" -> toJSON (removedPayments payment)
    "paymentTargets" -> toJSON (paymentTargets payment)
    "sealedTokens" -> toJSON (sealChaosTokenPayments payment)
    "actionsPaid" -> toJSON (totalActionPayment payment)
    "resourcesPaid" -> toJSON (totalResourcePayment payment)
    "cluesPaid" -> toJSON (totalCluePayment payment)
    "usesPaid" -> toJSON (totalUsesPayment payment)
    "damagePaid" -> toJSON (totalInvestigatorDamagePayment payment)
    "horrorPaid" -> toJSON (horrorPaid payment)
    "curseTokensPaid" -> toJSON (addedCurseTokenPayment payment)
    "cardsDiscarded" -> toJSON (totalDiscardCardPayments payment)
    _ -> Null

{- | What a set of icons is worth to the test being resolved.

Not a count: an icon the test does not want is worth nothing and a wild-minus is
worth -1, which is exactly what "add its matching icons to your skill value"
means and what counting them would get wrong.
-}

-- (implemented inline in 'evalExpr'; see the @iconValue@ case)

{- | A property of the skill test being resolved, which is not a value anything
binds -- there is only ever the one, and a card that talks about "matching"
icons or "succeed by" is talking about it.
-}
skillTestProp :: HasGame m => Text -> m Value
skillTestProp prop = case prop of
  "matchingIcons" -> toJSON . toList <$> getSkillTestMatchingSkillIcons
  "difficulty" -> toJSON <$> getSkillTestDifficulty
  "action" -> toJSON <$> getSkillTestAction
  "investigator" -> toJSON <$> getSkillTestInvestigator
  "committedCards" -> maybe Null (toJSON . concat . toList . skillTestCommittedCards) <$> getSkillTest
  "id" -> maybe Null (toJSON . skillTestId) <$> getSkillTest
  _ -> pure Null

{- | A property of one bound value. @kind@ says how to read it, the same way a
query's @kind@ says what its matcher matches: entity kinds name a 'Field' (the
names the engine already uses, @EnemyHealth@ and the like), and @card@ reads the
card def, which is not a projection.
-}
getProp :: HasGame m => Text -> Text -> Value -> m Value
getProp kind prop v = case kind of
  "enemy" -> entityProp @Enemy prop v
  "location" -> entityProp @Location prop v
  "investigator" -> entityProp @Investigator prop v
  "asset" -> entityProp @Asset prop v
  "act" -> entityProp @Act prop v
  -- The cost as it stands, discounts and all -- what a card means when it talks
  -- about "this card's resource cost" rather than the number printed on it.
  "card" | prop == "modifiedCost" -> case parseMaybe parseJSON v of
    Just card | Just iid <- toCardOwner card -> maybe Null toJSON <$> getModifiedCardCost iid card
    _ -> pure Null
  "card" -> pure $ cardProp prop v
  _ -> pure Null

entityProp
  :: forall a m
   . (HasGame m, Projection a, FromJSON (SomeField a), FromJSON (EntityId a))
  => Text
  -> Value
  -> m Value
entityProp prop v = case (parseMaybe parseJSON (String prop) :: Maybe (SomeField a), parseMaybe parseJSON v) of
  (Just (SomeField fld), Just eid) -> toJSON <$> field fld (eid :: EntityId a)
  _ -> pure Null

cardProp :: Text -> Value -> Value
cardProp prop v = case parseMaybe parseJSON v of
  Nothing -> Null
  Just card ->
    let def = toCardDef (card :: Card)
     in case prop of
          "icons" -> toJSON (cdSkills def)
          "skills" -> toJSON (cdSkills def)
          "traits" -> toJSON (toList $ cdCardTraits def)
          "class" -> toJSON (toList $ cdClassSymbols def)
          "name" -> toJSON (toTitle def)
          "cardCode" -> toJSON (toCardCode def)
          "cardType" -> toJSON (cdCardType def)
          "level" -> toJSON (cdLevel def)
          "cost" -> toJSON (cdCost def)
          -- The number on the card, for the readings that do arithmetic with it
          -- ("X is the cost of the event you discarded").
          "printedCost" -> toJSON (maybe 0 toPrintedCost (cdCost def))
          "id" -> toJSON (toCardId card)
          _ -> Null
