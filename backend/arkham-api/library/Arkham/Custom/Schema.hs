{-# LANGUAGE TemplateHaskell #-}

{- | A machine-readable description of the types a custom card's JSON is made of.

The ability editor builds windows, matchers, criteria and messages by picking a
constructor and filling in its fields. That is only possible if the client knows
what the constructors are, so this reifies them at compile time and serves the
result.

The closure is deliberately bounded: it expands the Arkham types worth editing
and stops at leaves the editor renders itself (text, numbers, ids). A type that
is referenced but not expanded still appears by name, and the editor falls back
to a raw JSON field for it.
-}
module Arkham.Custom.Schema (customSchema, TypeSchema (..), ConSchema (..), FieldSchema (..)) where

import Arkham.Ability.Limit
import Arkham.Ability.Type
import Arkham.Cost
import Arkham.Criteria
import Arkham.Custom.Schema.TH (schemaForWith)
import Arkham.Custom.Schema.Types
import Arkham.DamageEffect (DamageAssignment)
import Arkham.Discard (HandDiscard)
import Arkham.Discover (Discover)
import Arkham.Draw.Types (CardDraw)
import Arkham.Effect.Types (EffectBuilder)
import Arkham.Effect.Window (EffectWindow)
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.Enemy.Creation (EnemyCreation)
import Arkham.Evade.Types (ChooseEvade)
import Arkham.Exhaust (Exhaustion)
import Arkham.Fight.Types (ChooseFight)
import Arkham.Investigate.Types (Investigate)
import Arkham.Matcher
import Arkham.Message (Message)
import Arkham.Message.ChaosBag (ChaosBagMessage)
import Arkham.Message.Clue (ClueMessage)
import Arkham.Message.Damage (DamageMessage)
import Arkham.Message.Defeat (DefeatMessage)
import Arkham.Message.Doom (DoomMessage)
import Arkham.Message.EnemyAttack (EnemyAttackMessage)
import Arkham.Message.Engage (EngageMessage)
import Arkham.Message.Evade (EvadeMessage)
import Arkham.Message.Exhaust (ExhaustMessage)
import Arkham.Message.Fight (FightMessage)
import Arkham.Message.Horror (HorrorMessage)
import Arkham.Message.Hunt (HuntMessage)
import Arkham.Message.Investigator (InvestigatorMessage)
import Arkham.Message.Seal (SealMessage)
import Arkham.Message.Search (SearchMessage)
import Arkham.Message.SkillTest (SkillTestMessage)
import Arkham.Message.Spawn (SpawnMessage)
import Arkham.Message.Story (StoryMessage)
import Arkham.Message.Token (TokenMessage)
import Arkham.Modifier (Modifier, ModifierType)
import Arkham.Movement (Movement)
import Arkham.Search (Search)
import Arkham.SkillTest.Option (SkillTestOption)
import Arkham.Slot (Slot)
import Arkham.Source (Source)
import Arkham.Spawn (SpawnAt, SpawnDetails)
import Arkham.Strategy (ChosenCardStrategy, FindEncounterCardStrategy, ZoneReturnStrategy)
import Arkham.Target (Target)
import Arkham.Window (WindowType)

{- | Rooted at the types an ability is made of. 'Message' is listed but not
expanded through: it reaches most of the codebase, so the editor gets a picker
over every message and falls back to a raw field for whatever a message's own
fields turn out to be.
-}
customSchema :: [TypeSchema]
customSchema =
  $( schemaForWith
       [ ''AbilityType
       , ''AbilityLimit
       , ''Criterion
       , ''Cost
       , {- What a cost becomes once it is paid. Only ever a field of a message,
            and 'Message' is shallow, so nothing would reach it on its own and
            the editor would fall back to a raw JSON field for every payment a
            card writes. Everything it references is already here. -}
         ''Payment
       , ''WindowMatcher
       , {- The window an ability triggered on, whose positional fields its steps
            read as $w0, $w1, .... Listed so the editor can show what those are;
            which window a given matcher fires on is not derivable from either
            type (a third of the names differ), so the editor asks. -}
         ''WindowType
       , ''EnemyMatcher
       , ''LocationMatcher
       , ''PlacementMatcher
       , ''InvestigatorMatcher
       , ''AssetMatcher
       , ''TreacheryMatcher
       , ''EventMatcher
       , ''SkillMatcher
       , ''StoryMatcher
       , ''ActMatcher
       , ''AgendaMatcher
       , ''ExtendedCardMatcher
       , ''Source
       , ''Target
       , -- What a DealDamage carries: how much, from what, and whether the
         -- damage is direct. A single-constructor record, so it is written
         -- without a tag.
         ''DamageAssignment
       , -- What an Exhaust carries: who exhausts what, and what follows it.
         ''Exhaustion
       , ''ModifierType
       , -- Scoped modifiers are pushed as CreateWindowModifierEffect, so the
         -- editor needs the window that scopes them, and the wrapper that
         -- carries one (a bare ModifierType is not what the message takes).
         ''Modifier
       , ''EffectWindow
       , -- CreateEffect is how a card leaves something behind that acts later:
         -- messages to run when its window ends.
         ''EffectBuilder
       , ''EffectMetadata
       , ''PreyMatcher
       , ''SpawnAt
       , ''SkillTestMatcher
       , ''SkillTestResultMatcher
       , {- The payloads of the messages a card pushes. Each of these is a record
            or a small enum sitting in a 'Message' field, and 'Message' is a
            shallow root -- so without naming them here nothing reaches them and
            the editor offers a raw JSON box for the whole payload. Writing
            @discardStrategy@ and @discardAmount@ by hand is exactly what the
            editor exists to avoid. -}
         ''HandDiscard
       , ''Investigate
       , ''ChooseFight
       , ''ChooseEvade
       , ''Discover
       , ''Movement
       , ''CardDraw
       , ''Search
       , ''SpawnDetails
       , ''EnemyCreation
       , ''Slot
       , ''SkillTestOption
       , ''ChosenCardStrategy
       , ''ZoneReturnStrategy
       , ''FindEncounterCardStrategy
       ]
       ( ''Message
           -- Most messages sit inside a grouping constructor, and the editor has
           -- to see through it to name a message and its fields.
           : [ ''ChaosBagMessage
             , ''ClueMessage
             , ''DamageMessage
             , ''DefeatMessage
             , ''DoomMessage
             , ''EngageMessage
             , ''EnemyAttackMessage
             , ''EvadeMessage
             , ''ExhaustMessage
             , ''FightMessage
             , ''HorrorMessage
             , ''HuntMessage
             , ''InvestigatorMessage
             , ''SearchMessage
             , ''SealMessage
             , ''SkillTestMessage
             , ''SpawnMessage
             , ''StoryMessage
             , ''TokenMessage
             ]
       )
   )
