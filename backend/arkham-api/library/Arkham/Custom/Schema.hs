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
import Arkham.Effect.Types (EffectBuilder)
import Arkham.Effect.Window (EffectWindow)
import Arkham.EffectMetadata (EffectMetadata)
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
import Arkham.Source (Source)
import Arkham.Spawn (SpawnAt)
import Arkham.Target (Target)

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
       , ''WindowMatcher
       , ''EnemyMatcher
       , ''LocationMatcher
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
