{- | Expressions over what a custom card's steps have bound.

A @query@ is itself an expression, yielding a list of ids or cards (or, with a
@mode@, the first of them or how many there were). On its own that is all it can
say. A card that reads "draw 1 card for each different skill icon among the cards
committed" needs to go from that list to a property of each element and then to a
number, which is what this evaluates.

Shape: an expression is a JSON value. Anything that is not one of the operators
below is a literal (with @$bindings@ substituted), so numbers and @"$cards"@
mean themselves. An operator that takes @of@ broadcasts over a list, so @get@ is
both "the property of this one" and "map over these".
-}
module Arkham.Custom.Expr (evalExpr, exprInt, jsonField, runQuery, runQueryStep, valueList) where

import Arkham.Act.Types (Act)
import Arkham.Asset.Types (Asset)
import Arkham.CampaignLogKey (CampaignLogKey, Recorded (..), SomeRecorded (..))
import Arkham.Card
import Arkham.Classes.Entity (EntityId)
import Arkham.Classes.HasGame

-- Named, because this module has its own `matches` for expression predicates
-- and the whole module would shadow it into an ambiguity.
import Arkham.Classes.Query (Query, select)
import Arkham.Cost (
  Payment,
  addedCurseTokenPayment,
  chosenCardPayment,
  chosenEnemyPayment,
  chosenTraitPayment,
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
import Arkham.Custom.Env (Env, decodeWith)
import Arkham.Enemy.Types (Enemy, Field (EnemyKeywords))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Card (getModifiedCardCost)
import Arkham.Helpers.FetchCard (FetchCard (fetchCardMaybe_))
import Arkham.Helpers.Log (getRecordCount, getRecordSet)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Ref (targetToMaybeCard)
import Arkham.Helpers.SkillTest (
  getSkillTest,
  getSkillTestAction,
  getSkillTestDifficulty,
  getSkillTestInvestigator,
  getSkillTestMatchingSkillIcons,
  getSkillTestSkillTypes,
 )
import Arkham.Helpers.SkillTest.Target (getSkillTestTarget)
import Arkham.Homebrew.Defs (allTraits)
import Arkham.Id
import Arkham.Investigator.Types (Investigator)
import Arkham.Location.Types (Field (LocationKeywords), Location)
import Arkham.Matcher
import Arkham.Modifier (ModifierType (AddKeyword, Blank, RemoveKeyword))
import Arkham.Name (toTitle)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Query (QueryElement)
import Arkham.Skill.Types (Skill)
import Arkham.Target
import Arkham.Treachery.Types (Field (TreacheryKeywords))
import Arkham.SkillTest.Base (
  skillTestAdditionalRevealedChaosTokens,
  skillTestCommittedCards,
  skillTestIconValues,
  skillTestId,
  skillTestResult,
  skillTestRevealedChaosTokens,
  skillTestSource,
 )
import Arkham.SkillTestResult
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.List qualified as List
import Data.Text qualified as T

evalExpr :: HasGame m => Env -> Value -> m Value
evalExpr env v0 = case substituteExpr env v0 of
  Object o
    | Just prop <- str =<< KeyMap.lookup "get" o -> withOf o (getProp (kindOf o) prop)
    | Just prop <- str =<< KeyMap.lookup "map" o -> withOf o (getProp (kindOf o) prop)
    | Just p <- KeyMap.lookup "filter" o -> do
        p' <- evalPredicate p
        {- Keeping the elements rather than what was read off them. A plain
        filter compares each element itself, which is no use for a list of
        things with a property worth testing -- "the tokens that are not the
        {auto_fail}" is a question about each token's face, and the answer
        wanted is the tokens. @by@ says which property to read; without it the
        element IS the value, as before. -}
        case str =<< KeyMap.lookup "by" o of
          Nothing -> listOp o (filter (matches p'))
          Just prop -> case KeyMap.lookup "of" o of
            Nothing -> pure Null
            Just e -> do
              xs <- valueList <$> evalExpr env e
              -- With a `kind` it is a property the game is asked for; without
              -- one it is a field of the element's own JSON, which is how a
              -- chaos token's face is reached.
              let readKey = if null (kindOf o) then pure . jsonField prop else getProp (kindOf o) prop
              read' <- traverse readKey xs
              pure $ toJSON [x | (x, k) <- zip xs read', matches p' k]
    | Just prop <- str =<< KeyMap.lookup "skillTest" o -> skillTestProp prop
    {- A field of the JSON itself, rather than a property the game has to be
       asked for. An entity's own serialized values are bound whole -- @$placement@
       is @{"tag": "AttachedToLocation", "contents": <id>}@ -- so reaching what is
       inside one is otherwise impossible. An index reads into a list, which is
       what a constructor with more than one field serializes as. -}
    | Just key <- str =<< KeyMap.lookup "field" o -> case KeyMap.lookup "of" o of
        Nothing -> pure Null
        Just e -> jsonField key <$> evalExpr env e
    {- A query is a value like any other, so it can be bound by @let@ and
       transformed in place rather than needing a step of its own. The matcher is
       decoded against the environment, so it can refer to bindings. -}
    | Just q <- KeyMap.lookup "query" o ->
        fromMaybe Null <$> runQueryStep env q (KeyMap.lookup "mode" o)
    {- What the campaign log remembers.

       The log is not the board, so no matcher reaches it, and a card whose X is
       "half the traits you have learned" has nothing else to count. A set yields
       its values so they can be counted, filtered and compared like any other
       list; a count yields the number kept under that key, which is a different
       thing the log stores and not the length of the set. -}
    | Just key <- KeyMap.lookup "recordSet" o ->
        withKey key (fmap (toJSON . recordedValues) . getRecordSet)
    | Just key <- KeyMap.lookup "recordCount" o -> withKey key (fmap toJSON . getRecordCount)
    | Just name <- str =<< KeyMap.lookup "apply" o ->
        applyFn name (str =<< KeyMap.lookup "kind" o)
          =<< evalExpr env (fromMaybe Null (KeyMap.lookup "to" o))
    | Just e <- KeyMap.lookup "iconValue" o -> do
        icons <- valueList <$> evalExpr env e
        values <- maybe mempty skillTestIconValues <$> getSkillTest
        let valueOf v = maybe 0 (\i -> findWithDefault 0 i values) (parseMaybe parseJSON v)
        pure $ toJSON (sum (map valueOf icons))
    {- The same value, a worked-out number of times. Nothing else in the
    language builds a list of a given length, and several cards need one: "gains
    one {wild} for every 2 clues you control" is a list of icons whose length the
    game decides, and the modifier that takes it wants the list, not the count. -}
    {- Every trait there is, so "name a [[Trait]]" is a choice over a list like
    any other rather than a step of its own. Homebrew traits are included: the
    list is the one the engine itself offers. -}
    | Just _ <- KeyMap.lookup "traits" o -> pure $ toJSON allTraits
    | Just e <- KeyMap.lookup "replicate" o -> do
        n <- maybe (pure 0) (exprInt env) (KeyMap.lookup "times" o)
        x <- evalExpr env e
        pure $ toJSON (List.replicate (max 0 n) x)
    | Just e <- KeyMap.lookup "unique" o -> unary e (List.nub . valueList)
    | Just e <- KeyMap.lookup "concat" o -> unary e (concatMap valueList . valueList)
    | Just e <- KeyMap.lookup "reverse" o -> unary e (reverse . valueList)
    {- The first @n@, for the readings that are "4 of them" rather than "all of
    them": a card that seals four random tokens draws more than four and keeps
    the first four it may have. -}
    | Just e <- KeyMap.lookup "take" o -> do
        n <- maybe (pure 0) (exprInt env) (KeyMap.lookup "times" o)
        unary e (List.take (max 0 n) . valueList)
    | Just e <- KeyMap.lookup "count" o -> number e (length . valueList)
    | Just e <- KeyMap.lookup "sum" o -> number e (sum . nums)
    | Just e <- KeyMap.lookup "max" o -> number e (safely List.maximum . nums)
    | Just e <- KeyMap.lookup "min" o -> number e (safely List.minimum . nums)
    | Just e <- KeyMap.lookup "first" o -> unaryV e (fromMaybe Null . headMay . valueList)
    | Just e <- KeyMap.lookup "add" o -> number e (sum . nums)
    | Just e <- KeyMap.lookup "multiply" o -> number e (product . nums)
    | Just e <- KeyMap.lookup "subtract" o -> number e (pairwise (-))
    | Just e <- KeyMap.lookup "divide" o -> number e (pairwise safeDiv)
  {- An operand list, each entry an expression in its own right.

     @{"add": [<how many traits you have learned>, 1]}@ is what the editor builds
     -- it renders a whole expression per operand -- and the arithmetic operators
     read their list through 'nums', which scores an unevaluated object as zero.
     So without this an expression over anything but literals answers 0, and says
     nothing about having done so. -}
  Array xs -> Array <$> traverse (evalExpr env) xs
  v -> pure v
 where
  -- A predicate's operand is an expression too, so @{"in": {"skillTest": ...}}@
  -- compares against what that works out to rather than against the literal.
  evalPredicate = \case
    Object po -> Object <$> traverse (evalExpr env) po
    p -> evalExpr env p
  -- Already substituted, so the key is read as written rather than substituted
  -- a second time.
  withKey key f = maybe (pure Null) f (parseMaybe parseJSON key :: Maybe CampaignLogKey)
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

{- | The values a record set holds, as JSON.

Crossed out is the log's way of saying "this is no longer true", so those are
dropped -- a crossed-out trait is one you have unlearned and should not count
towards anything. A circle is an annotation on a value that /is/ still recorded,
so it is looked through rather than dropped.
-}
recordedValues :: [SomeRecorded] -> [Value]
recordedValues = mapMaybe recordedValue

recordedValue :: SomeRecorded -> Maybe Value
recordedValue (SomeRecorded _ rec) = go rec
 where
  go = \case
    Recorded r -> Just (toJSON r)
    Circled inner -> go inner
    CrossedOut _ -> Nothing

-- | An expression whose value is wanted as a count.
exprInt :: HasGame m => Env -> Value -> m Int
exprInt env = fmap toInt . evalExpr env

{- | What @field@ reads: a key of an object, or -- when the key is a number --
that position in a list.
-}

{- | A field of a serialized value, by the name a card author would write.

Every Haskell record prefixes its fields with its own type -- a chaos token's
face is @chaosTokenFace@, not @face@ -- which is a name nobody writing a card
knows. So a miss falls back to the one field whose name ends with the key: one
and only one, because two would be a guess rather than a reading.
-}
jsonField :: Text -> Value -> Value
jsonField key = \case
  Object o -> case KeyMap.lookup (Key.fromText key) o of
    Just v -> v
    Nothing -> case [v | (k, v) <- KeyMap.toList o, Key.toText k `endsWithKey` key] of
      [v] -> v
      _ -> Null
  Array xs | Just i <- readMay (T.unpack key) -> fromMaybe Null (toList xs !!? i)
  _ -> Null
 where
  endsWithKey candidate k = case T.uncons k of
    Just (c, rest) -> (T.toUpper (T.singleton c) <> rest) `T.isSuffixOf` candidate
    Nothing -> False

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
applyFn :: HasGame m => Text -> Maybe Text -> Value -> m Value
applyFn name kind v = case name of
  "fetchCard" -> fetchCardOf kind v
  {- The engine holds one skill test at a time, so an id can only name the test
     being resolved. Anything else -- a test that has ended, a test in another
     player's window -- is gone and yields Null rather than a stale answer. -}
  "getSkillTest" -> do
    current <- getSkillTest
    pure $ case (parseMaybe parseJSON v, current) of
      (Just sid, Just st) | sid == skillTestId st -> toJSON st
      _ -> Null
  _ -> pure $ case parseMaybe parseJSON v of
    Nothing -> Null
    Just payment -> paymentFn name payment

{- | The card behind an id, for anything with a 'FetchCard' instance.

Which kind of id it is has to be said, because it cannot be read back off the
value: every id serializes as a bare uuid, so an @EnemyId@ and a @CardId@ are
the same JSON and trying each in turn would answer with whichever was tried
first. The editor knows the type it is transforming, so it writes the kind into
the stage.

'fetchCardMaybe_' rather than 'fetchCard': it asks only for 'HasGame', and an id
naming nothing yields Null the way every other reading here does instead of
bringing the game down.
-}
fetchCardOf :: forall m. HasGame m => Maybe Text -> Value -> m Value
fetchCardOf kind v = case kind of
  Just "asset" -> go @AssetId
  Just "event" -> go @EventId
  Just "treachery" -> go @TreacheryId
  Just "enemy" -> go @EnemyId
  Just "location" -> go @LocationId
  Just "story" -> go @StoryId
  _ -> go @CardId
 where
  go :: forall a. (FromJSON a, FetchCard a) => m Value
  go = case parseMaybe parseJSON v of
    Nothing -> pure Null
    Just x -> maybe Null toJSON <$> fetchCardMaybe_ (x :: a)

-- | The readings of a 'Payment', which need nothing but the payment itself.
paymentFn :: Text -> Payment -> Value
paymentFn name payment = case name of
  "paidCards" -> toJSON (discardedCards payment)
  "discardedCard" -> toJSON (discardPayment payment)
  "chosenCard" -> toJSON (chosenCardPayment payment)
  "chosenTrait" -> toJSON (chosenTraitPayment payment)
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
  {- By how much it went, as a plain number: positive for a success, negative
  for a failure, zero for a test that has not been resolved. "For every 2 points
  you succeed by" has no other way to count. -}
  "margin" ->
    getSkillTest <&> \case
      Just st -> toJSON $ case skillTestResult st of
        SucceededBy _ n -> n
        FailedBy _ n -> negate n
        Unrun -> 0
      Nothing -> toJSON (0 :: Int)
  "difficulty" -> toJSON <$> getSkillTestDifficulty
  "action" -> toJSON <$> getSkillTestAction
  "investigator" -> toJSON <$> getSkillTestInvestigator
  "committedCards" -> maybe Null (toJSON . concat . toList . skillTestCommittedCards) <$> getSkillTest
  {- What the test is ABOUT -- the enemy being fought, the location being
  investigated -- as the target it is. A card that reads something off the thing
  it is testing against ("draw 1 card for each keyword on the target card") has
  no other way to name it. -}
  "target" -> maybe Null toJSON <$> getSkillTestTarget
  {- And what is DOING the testing -- the card the test was initiated from. The
  target says what the test is about; this says what it is being made with, which
  is what a card that answers "the source of that test" has to read. -}
  "source" -> maybe Null (toJSON . skillTestSource) <$> getSkillTest
  {- Which skills the test uses. A card that says "the same skill" -- the next
  test another investigator takes using it -- has to read one off the test it
  is talking about; a test may use more than one, so this is a list. -}
  "skills" -> toJSON <$> getSkillTestSkillTypes
  {- Every token the test has turned over, additional reveals included. A card
  that does something with "each token revealed during this test" has nowhere
  else to read them from. -}
  "revealedTokens" ->
    getSkillTest <&> \case
      Just st ->
        toJSON (skillTestRevealedChaosTokens st <> skillTestAdditionalRevealedChaosTokens st)
      Nothing -> toJSON ([] :: [Value])
  "id" -> maybe Null (toJSON . skillTestId) <$> getSkillTest
  _ -> pure Null

{- | A property of one bound value. @kind@ says how to read it, the same way a
query's @kind@ says what its matcher matches: entity kinds name a 'Field' (the
names the engine already uses, @EnemyHealth@ and the like), and @card@ reads the
card def, which is not a projection.
-}
getProp :: HasGame m => Text -> Text -> Value -> m Value
getProp kind prop v = case kind of
  {- Read off the id rather than off a materialised test: the readings that
     matter -- what icons match, what the difficulty is now -- are computed from
     the game, not fields sitting on the record. -}
  "skillTest" -> do
    current <- getSkillTest
    case (parseMaybe parseJSON v, current) of
      (Just sid, Just st) | sid == skillTestId st -> skillTestProp prop
      _ -> pure Null
  {- Whatever the target names, worked out when the reading is made.
  Every other kind has to be written down in advance, and the thing a skill test
  is about cannot be: it is an enemy for a fight, a location for an investigate,
  a treachery for the test a treachery makes you take. A 'Target' says which it
  is, so nothing has to be declared. -}
  "target" -> targetProp prop v
  "enemy" -> entityProp @Enemy prop v
  "location" -> entityProp @Location prop v
  "investigator" -> entityProp @Investigator prop v
  "asset" -> entityProp @Asset prop v
  -- A skill is an entity like the rest once it is on the table rather than in
  -- a hand: attached to a location, or sitting on a test.
  "skill" -> entityProp @Skill prop v
  "act" -> entityProp @Act prop v
  -- The cost as it stands, discounts and all -- what a card means when it talks
  -- about "this card's resource cost" rather than the number printed on it.
  "card" | prop == "modifiedCost" -> case parseMaybe parseJSON v of
    Just card | Just iid <- toCardOwner card -> maybe Null toJSON <$> getModifiedCardCost iid card
    _ -> pure Null
  "card" -> pure $ cardProp prop v
  _ -> pure Null

{- | A reading off whatever a 'Target' names.

@keywords@ is asked of the thing itself -- an enemy that was granted Retaliate
has Retaliate, and a blanked one has none of what it printed -- and falls back
to the card when the thing has gone, which is the ordinary case for a card that
reads the enemy a successful fight has just defeated. Every other reading is a
reading of the card, which is the one thing every target has.
-}
targetProp :: HasGame m => Text -> Value -> m Value
targetProp prop v = case parseMaybe parseJSON v :: Maybe Target of
  Nothing -> pure Null
  Just target -> go target
 where
  go = \case
    ProxyTarget t _ -> go t
    -- Two things in one target (an enemy and its location, say). The first that
    -- answers is the answer, which is how 'targetToMaybeCard' reads one too.
    BothTarget a b ->
      go a >>= \case
        Null -> go b
        answered -> pure answered
    target -> case prop of
      -- The three that keep their keywords as a field of their own are asked
      -- for it, since each has rules the others do not (an enemy's are gone
      -- when it is blanked, a location's are read through its revealed side).
      "keywords" -> case target of
        EnemyTarget eid -> orComputed target . fmap toJSON =<< fieldMay EnemyKeywords eid
        LocationTarget lid -> orComputed target . fmap toJSON =<< fieldMay LocationKeywords lid
        TreacheryTarget tid -> orComputed target . fmap toJSON =<< fieldMay TreacheryKeywords tid
        _ -> orComputed target Nothing
      _ -> maybe Null (cardProp prop . toJSON) <$> targetToMaybeCard target

  {- Everything else has keywords too -- an asset is Permanent or Bonded, a
  player card is Myriad or Exceptional -- they are simply not a field of its
  own, and neither is anything's once it has left play. The same reading the
  three fields do, done here from the card and the modifiers standing on it:
  what it prints, dropped if it is blanked, plus what it has been granted and
  less what has been taken away. -}
  orComputed target = \case
    Just answered -> pure answered
    Nothing ->
      targetToMaybeCard target >>= \case
        Nothing -> pure Null
        Just card -> do
          modifiers' <- foldMapM getModifiers [target, CardIdTarget (toCardId card)]
          let
            printed = if Blank `elem` modifiers' then [] else toList (cdKeywords (toCardDef card))
            granted = [k | AddKeyword k <- modifiers']
            taken = [k | RemoveKeyword k <- modifiers']
          pure $ toJSON $ List.nub [k | k <- printed <> granted, k `notElem` taken]

entityProp
  :: forall a m
   . (HasGame m, Projection a, FromJSON (SomeField a), FromJSON (EntityId a))
  => Text
  -> Value
  -> m Value
entityProp prop v = case (parseMaybe parseJSON (String prop) :: Maybe (SomeField a), parseMaybe parseJSON v) of
  {- Asked of whatever id it is handed, which is not always a thing that is
  there: ids are UUIDs, so a location's reads as an enemy's just as well, and
  a card that counts the keywords on the skill test's target meets one the
  moment that test is an investigate -- or the moment the enemy it was about
  is defeated before the window it reads in. @field@ on a thing that has gone
  is an error that takes the whole game with it, so the reading is Null, the
  same as every other property nothing answers. -}
  (Just (SomeField fld), Just eid) -> maybe Null toJSON <$> fieldMay fld (eid :: EntityId a)
  _ -> pure Null

{- | A reading off a card.

Takes a bare 'PlayerCard' as well as a 'Card': the messages that hand a card
back -- what a discard took off the top of a deck, what a search found -- carry
the unwrapped form, and a card is a card whichever way it arrived.
-}
cardProp :: Text -> Value -> Value
cardProp prop v = case asCard v of
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
          "subType" -> toJSON (cdCardSubType def)
          -- Whose card it is, which is not a property of the def: a card in a
          -- discard pile has to be put back into *that* player's deck.
          "owner" -> toJSON (toCardOwner card)
          _ -> Null

asCard :: Value -> Maybe Card
asCard v =
  parseMaybe parseJSON v
    <|> (PlayerCard <$> parseMaybe parseJSON v)
    <|> (EncounterCard <$> parseMaybe parseJSON v)

{- | @mode@ decides what a query binds: the whole list (the default), just the
first element, or how many there were.
-}
runQueryStep :: HasGame m => Env -> Value -> Maybe Value -> m (Maybe Value)
runQueryStep env q mode = fmap (fmap shape) (runQuery env q)
 where
  shape results = case mode of
    Just (String "first") -> fromMaybe Null (headMay results)
    Just (String "count") -> toJSON (length results)
    _ -> toJSON results

-- | Dispatch a @{"kind": …, "matcher": …}@ query onto the matcher it names.
runQuery :: HasGame m => Env -> Value -> m (Maybe [Value])
runQuery env v = case v of
  Object o -> case (KeyMap.lookup "kind" o, KeyMap.lookup "matcher" o) of
    (Just (String kind), Just matcher) -> case kind of
      "enemy" -> run @EnemyMatcher matcher
      "location" -> run @LocationMatcher matcher
      "investigator" -> run @InvestigatorMatcher matcher
      "asset" -> run @AssetMatcher matcher
      "treachery" -> run @TreacheryMatcher matcher
      "event" -> run @EventMatcher matcher
      "skill" -> run @SkillMatcher matcher
      "story" -> run @StoryMatcher matcher
      "act" -> run @ActMatcher matcher
      "agenda" -> run @AgendaMatcher matcher
      "card" -> run @ExtendedCardMatcher matcher
      {- Chaos tokens are entities a card can reach -- sealed on it, revealed by
         you -- and the only way to say "each token sealed on that card". -}
      "chaosToken" -> run @ChaosTokenMatcher matcher
      _ -> pure Nothing
    _ -> pure Nothing
  _ -> pure Nothing
 where
  run
    :: forall a m'
     . (HasGame m', FromJSON a, Query a, ToJSON (QueryElement a))
    => Value
    -> m' (Maybe [Value])
  run matcher = case decodeWith env matcher of
    Nothing -> pure Nothing
    Just m -> Just . map toJSON <$> select (m :: a)
