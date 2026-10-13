{- | What a custom card set brings besides its cards: its own tokens, and its
own keywords.

An /investigator expansion/ is a set that comes as a box rather than a handful
of cards, and a box brings furniture. Circus Ex Mortis prints a Tension keyword
on forty of its player cards and ships a Tension token to mark it with, and
neither of those is a card. They are declared once on the set and stamped onto
every card in it ('setTokensMetaKey', 'setKeywordsMetaKey'), so a card exported
on its own still says what its keyword means and what its token looks like --
the same reason a card carries the name of its set.

A token here is a /marker a card places on itself/, not a chaos token. Chaos
tokens belong to a scenario and come from "Arkham.Homebrew.Tokens"; these are
counters, in the same family as charges and secrets, and reach the engine as
@Token.Custom "tension"@.

== A keyword is behaviour, not a label

The engine knows nothing about Tension. A keyword declares its own mechanics, as
data, in 'customSetKeywordBehavior' -- so a set can invent a keyword the engine
has never heard of without a line of Haskell. The description is what a player
reads; the behaviour is what the engine runs.

The behaviour is a /template/. It is written once on the set, in the same shapes
a card's own @_abilities@, @_modifiers@ and @_handlers@ already use, with
@$slot@ standing where each card differs. Tension's whole mechanism --

> When a card with Tension enters play, its condition is active but its result
> is not. After the card's controller satisfies the condition, the result
> immediately activates. [...] place a Tension token on the card as it
> activates.

-- is one ability template and one @activeWhen@ criterion:

> "slots": [{"name": "condition", "kind": "window", "required": true}, …]
> "behavior": {
>   "abilities": [
>     { "type": {"tag": "SilentForcedAbility", "window": "$condition"},
>       "criteria": {"tag": "Criteria", "contents": [
>         {"tag": "TokensOnThis", "contents": ["$keywordToken", {"tag": "EqualTo", …0}]},
>         "$requires"]},
>       "steps": [{"push": … PlaceTokens $keywordToken 1 …}] }
>   ],
>   "activeWhen": {"tag": "TokensOnThis", "contents": ["$keywordToken", {"tag": "GreaterThan", …0}]}
> }

A card then says only what is printed on it: which keyword, its own condition,
and its own result. 'withKeywordEffects' fills the template in and appends the
parts, so by the time anything else reads the def there is no keyword left --
only the ordinary abilities and modifiers the engine already runs.

Silent, because nothing is printed to activate: the result "immediately
activates", and a Forced prompt would be telling the player about an ability
their card does not have.

@activeWhen@ is what gates the card's own half of the result, and is the reason
the rest of the Tension rule comes out for free: the token carries the state,
tokens go when a card leaves play, so a result deactivates with it and a card
that comes back has to earn it again.
-}
module Arkham.Custom.SetDefs (
  CustomSetToken (..),
  CustomSetKeyword (..),
  KeywordSlot (..),
  KeywordBehavior (..),
  KeywordEffect (..),
  setTokensMetaKey,
  setKeywordsMetaKey,
  keywordEffectsMetaKey,
  keywordSlug,
  withKeywordEffects,
) where

import Arkham.Card.CardDef
import Arkham.Keyword (Keyword (CustomKeyword))
import Arkham.Prelude
import Arkham.Token (Token (Custom))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | Where a set's tokens are stamped onto each of its cards.
setTokensMetaKey :: Text
setTokensMetaKey = "setTokens"

-- | Where a set's keywords are stamped onto each of its cards.
setKeywordsMetaKey :: Text
setKeywordsMetaKey = "setKeywords"

{- | Where a card says which of its set's keywords it prints, and fills in the
slots that keyword asked for.
-}
keywordEffectsMetaKey :: Text
keywordEffectsMetaKey = "_keywords"

{- | A token a set brings with it.

@image@ is only ever shown, so it is optional: a token with no art still counts,
and the client falls back to a plain marker. The slug is what the engine stores
('Token.Custom'), and the name is what a player reads.
-}
data CustomSetToken = CustomSetToken
  { customSetTokenSlug :: Text
  , customSetTokenName :: Text
  , customSetTokenImage :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

instance FromJSON CustomSetToken where
  parseJSON = withObject "CustomSetToken" \o ->
    CustomSetToken
      <$> o
      .: "slug"
      <*> o
      .:? "name"
      .!= ""
      <*> o
      .:? "image"

instance ToJSON CustomSetToken where
  toJSON t =
    object
      $ ["slug" .= customSetTokenSlug t, "name" .= customSetTokenName t]
      <> ["image" .= img | img <- maybeToList (customSetTokenImage t)]

{- | One thing a keyword asks each card to fill in.

@kind@ is for whoever is editing the card -- it decides which editor the builder
puts there -- and means nothing to the engine, which only ever substitutes
whatever was filled in.

@required@ is the one part the engine does read: a card that leaves a required
slot empty gets nothing from the keyword at all, rather than a template with a
hole in it that fails to decode and vanishes without a word.
-}
data KeywordSlot = KeywordSlot
  { keywordSlotName :: Text
  , keywordSlotLabel :: Text
  , keywordSlotKind :: Text
  , keywordSlotRequired :: Bool
  }
  deriving stock (Show, Eq, Ord, Data, Generic)

instance FromJSON KeywordSlot where
  parseJSON = withObject "KeywordSlot" \o -> do
    name <- o .: "name"
    KeywordSlot name
      <$> o
      .:? "label"
      .!= name
      <*> o
      .:? "kind"
      .!= "value"
      <*> o
      .:? "required"
      .!= False

instance ToJSON KeywordSlot where
  toJSON s =
    object
      [ "name" .= keywordSlotName s
      , "label" .= keywordSlotLabel s
      , "kind" .= keywordSlotKind s
      , "required" .= keywordSlotRequired s
      ]

{- | What a keyword does, as templates in the shapes the engine already runs.

@abilities@, @modifiers@ and @handlers@ are appended to the card's own, with
@$slot@ replaced by what the card filled in.

@activeWhen@ is a criterion gating the card's /own/ result -- the @modifiers@
and @abilities@ on its @_keywords@ entry. A keyword whose result is immediate
rather than lasting leaves it out, and those parts then hold unconditionally.
-}
data KeywordBehavior = KeywordBehavior
  { keywordBehaviorAbilities :: [Value]
  , keywordBehaviorModifiers :: [Value]
  , keywordBehaviorHandlers :: [Value]
  , keywordBehaviorActiveWhen :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON KeywordBehavior where
  parseJSON = withObject "KeywordBehavior" \o ->
    KeywordBehavior
      <$> o
      .:? "abilities"
      .!= []
      <*> o
      .:? "modifiers"
      .!= []
      <*> o
      .:? "handlers"
      .!= []
      <*> o
      .:? "activeWhen"

instance ToJSON KeywordBehavior where
  toJSON b =
    object
      $ [ "abilities" .= keywordBehaviorAbilities b
        , "modifiers" .= keywordBehaviorModifiers b
        , "handlers" .= keywordBehaviorHandlers b
        ]
      <> ["activeWhen" .= w | w <- maybeToList (keywordBehaviorActiveWhen b)]

{- | A keyword a set prints on its own cards.

A keyword with no @behavior@ is rules text and nothing more: the cards that
print it say what they do in the ordinary way, and this is here so a player can
read what the word means.
-}
data CustomSetKeyword = CustomSetKeyword
  { customSetKeywordName :: Text
  , customSetKeywordDescription :: Text
  -- ^ The rules text, as the box prints it.
  , customSetKeywordToken :: Maybe Text
  {- ^ The token its behaviour marks itself with, bound as @$keywordToken@.
  Defaults to 'keywordSlug' of the name, so a set that declares a keyword and
  a token of the same name needs to say nothing.
  -}
  , customSetKeywordSlots :: [KeywordSlot]
  , customSetKeywordBehavior :: Maybe KeywordBehavior
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CustomSetKeyword where
  parseJSON = withObject "CustomSetKeyword" \o ->
    CustomSetKeyword
      <$> o
      .: "name"
      <*> o
      .:? "description"
      .!= ""
      <*> o
      .:? "token"
      <*> o
      .:? "slots"
      .!= []
      <*> o
      .:? "behavior"

instance ToJSON CustomSetKeyword where
  toJSON k =
    object
      $ [ "name" .= customSetKeywordName k
        , "description" .= customSetKeywordDescription k
        , "slots" .= customSetKeywordSlots k
        ]
      <> ["token" .= t | t <- maybeToList (customSetKeywordToken k)]
      <> ["behavior" .= b | b <- maybeToList (customSetKeywordBehavior k)]

{- | The slug a name becomes when nobody gave one: lowercased, with everything
that is not a letter or a digit dropped. @"Tension"@ is @"tension"@.
-}
keywordSlug :: Text -> Text
keywordSlug = T.filter isAlphaNum . T.toLower

{- | One keyword as a card prints it: which keyword, and what this card fills
its slots with.

Beyond @keyword@ and @token@, every key is a slot the keyword declared, so what
a card is asked for is the keyword's business rather than something listed here.
@modifiers@ and @abilities@ are the exception: they are the card's own half of
the result, gated by the keyword's @activeWhen@ rather than substituted into it.
-}
data KeywordEffect = KeywordEffect
  { keywordEffectKeyword :: Text
  , keywordEffectToken :: Maybe Text
  , keywordEffectFills :: Map Text Value
  , keywordEffectModifiers :: [Value]
  , keywordEffectAbilities :: [Value]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON KeywordEffect where
  parseJSON = withObject "KeywordEffect" \o -> do
    keyword <- o .:? "keyword" .!= ""
    token <- o .:? "token"
    modifiers <- o .:? "modifiers" .!= []
    abilities <- o .:? "abilities" .!= []
    let fills =
          Map.fromList
            [ (Key.toText k, v)
            | (k, v) <- KeyMap.toList o
            , Key.toText k `notElem` (["keyword", "token", "modifiers", "abilities"] :: [Text])
            ]
    pure $ KeywordEffect keyword token fills modifiers abilities

{- | Turn every @_keywords@ entry into the parts the engine already runs.

Appended rather than merged in, so the card's own abilities keep the indices
they were written with: "the above [reaction] ability" means the same ability
after this as before it.
-}
withKeywordEffects :: CardDef -> CardDef
withKeywordEffects def
  | null entries = def
  | otherwise = def {cdMeta = meta', cdKeywords = cdKeywords def <> stamped}
 where
  {- The keyword's NAME, as a keyword the engine carries but acts on no further.
  Everything the keyword does is expanded below into ordinary abilities and
  modifiers; this is so another card can ask whether this one has the keyword
  at all -- "an asset you control with a Tension effect" -- which is otherwise
  a question about printed text that no matcher reaches. -}
  stamped =
    setFromList
      [CustomKeyword (keywordEffectKeyword entry) | entry <- entries, declares entry]
  declares entry =
    any ((== keywordEffectKeyword entry) . customSetKeywordName) keywords

  entries :: [KeywordEffect]
  entries = fromMaybe [] (metaList keywordEffectsMetaKey)

  keywords :: [CustomSetKeyword]
  keywords = fromMaybe [] (metaList setKeywordsMetaKey)

  metaList :: FromJSON a => Text -> Maybe [a]
  metaList key = Map.lookup key (cdMeta def) >>= parseMaybe parseJSON

  parts = map generated entries

  meta' =
    appendMeta "_abilities" (concatMap fst3 parts)
      $ appendMeta "_modifiers" (concatMap snd3 parts)
      $ appendMeta "_handlers" (concatMap thd3 parts) (cdMeta def)

  fst3 (a, _, _) = a
  snd3 (_, b, _) = b
  thd3 (_, _, c) = c

  appendMeta key added meta
    | null added = meta
    | otherwise = Map.insert key (toJSON (existing <> added)) meta
   where
    existing = fromMaybe [] (Map.lookup key meta >>= parseMaybe parseJSON)

  {- What one entry contributes: the keyword's own templates, filled in, and the
  card's result gated by the keyword's @activeWhen@. A keyword the set does not
  declare -- a card carrying a stamp from a set it has since left -- contributes
  nothing, which is the same as not printing it. -}
  generated :: KeywordEffect -> ([Value], [Value], [Value])
  generated entry = case find named keywords of
    Nothing -> ([], [], [])
    Just keyword
      | not (satisfied keyword entry) -> ([], [], [])
      | otherwise ->
          let fill = substituteSlots (env keyword entry) (declaredSlots keyword)
              behavior = fromMaybe emptyBehavior (customSetKeywordBehavior keyword)
              gate = keywordBehaviorActiveWhen behavior >>= fill
           in ( mapMaybe fill (keywordBehaviorAbilities behavior)
                  <> map (gateAbility gate) (keywordEffectAbilities entry)
              , mapMaybe fill (keywordBehaviorModifiers behavior)
                  <> map (gateModifier gate) (keywordEffectModifiers entry)
              , mapMaybe fill (keywordBehaviorHandlers behavior)
              )
   where
    named k = customSetKeywordName k == keywordEffectKeyword entry

  emptyBehavior = KeywordBehavior [] [] [] Nothing

  -- Every slot the keyword insists on has been filled in.
  satisfied keyword entry =
    and
      [ Map.member (keywordSlotName slot) (keywordEffectFills entry)
      | slot <- customSetKeywordSlots keyword
      , keywordSlotRequired slot
      ]

  declaredSlots keyword =
    "keywordToken"
      : "keywordName"
      : map keywordSlotName (customSetKeywordSlots keyword)

  {- What the templates may refer to: the keyword's token and name, and whatever
  the card filled in. Nothing else is touched, so the runtime bindings a step
  reads later -- @$source@, @$id@, @$controller@ -- pass through untouched. -}
  env keyword entry =
    Map.insert "keywordToken" (toJSON (Custom (tokenOf keyword entry)))
      $ Map.insert "keywordName" (toJSON (customSetKeywordName keyword))
      $ keywordEffectFills entry

  tokenOf keyword entry =
    fromMaybe
      (fromMaybe (keywordSlug (customSetKeywordName keyword)) (customSetKeywordToken keyword))
      (keywordEffectToken entry)

-- | An ability spec with the keyword's gate on it, keeping its own criteria.
gateAbility :: Maybe Value -> Value -> Value
gateAbility Nothing v = v
gateAbility (Just gate) v = case v of
  Object o -> Object $ KeyMap.insert "criteria" merged o
   where
    merged = allCriteria (gate : maybeToList (KeyMap.lookup "criteria" o))
  _ -> v

{- | A modifier spec with the keyword's gate on it. @if@ reads a list as "all of
these", so this is a prepend rather than a merge.
-}
gateModifier :: Maybe Value -> Value -> Value
gateModifier Nothing v = v
gateModifier (Just gate) v = case v of
  Object o -> Object $ KeyMap.insert "if" (toJSON (wrapped : existing)) o
   where
    wrapped = object ["criteria" .= gate]
    existing = case KeyMap.lookup "if" o of
      Just (Array xs) -> toList xs
      Just other -> [other]
      Nothing -> []
  _ -> v

-- | Several criteria as one. A single criterion is left as it is.
allCriteria :: [Value] -> Value
allCriteria = \case
  [one] -> one
  many' -> object ["tag" .= String "Criteria", "contents" .= many']

{- | Fill a template in: @$slot@ becomes what the card put there, and a slot the
card left empty takes with it whatever stood for it.

Only the names the keyword declared are touched. @$source@, @$id@, @$iid@ and
the rest are bindings the steps resolve when they run, long after this, and
substituting them here would replace them with nothing.

An unfilled slot leaves a hole, and a hole is removed rather than written as
null: dropped from a list, and dropped along with the key it was the value of.
A whole template that /is/ a hole is dropped, which is 'Nothing' here.

A slot standing where a list element goes is /spliced/ when what filled it is
itself a list. That is what lets a template say @"steps": [<place the token>,
"$steps"]@ and mean "and then whatever else this card does", rather than nesting
the card's steps inside the list as one unreadable element. It only ever applies
where a list was already being built, so a slot holding a single value is
unaffected.
-}
substituteSlots :: Map Text Value -> [Text] -> Value -> Maybe Value
substituteSlots filled declared = go
 where
  go :: Value -> Maybe Value
  go = \case
    String t
      | Just name <- T.stripPrefix "$" t
      , name `elem` declared ->
          Map.lookup name filled
    Object o -> Just $ Object (KeyMap.mapMaybe go o)
    Array xs -> Just $ toJSON (concatMap element (toList xs))
    v -> Just v

  element v = case go v of
    Nothing -> []
    Just (Array ys) | isSlot v -> toList ys
    Just filled' -> [filled']

  isSlot = \case
    String t | Just name <- T.stripPrefix "$" t -> name `elem` declared
    _ -> False
