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
import Arkham.Effect.Window (EffectWindow)
import Arkham.Matcher
import Arkham.Message (Message)
import Arkham.Modifier (ModifierType)
import Arkham.Source (Source)
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
         -- editor needs the window that scopes them and who they apply to.
         ''EffectWindow
       , ''PreyMatcher
       , ''SkillTestMatcher
       , ''SkillTestResultMatcher
       ]
       [''Message]
   )
