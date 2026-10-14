{-# LANGUAGE ImplicitParams #-}

{- | Community narrative variants: rewrites of a campaign's prose, selected at
creation and changing nothing mechanically.

A variant is a locale namespace plus, where its structure differs from the
printed guide's, a handful of overrides. The client merges the variant's messages
over the campaign's own, so a block the variant only rewords needs nothing here
-- it resolves to the variant's prose at the key the engine already emits. These
combinators exist for the rest: a block a variant splits, replaces, or adds.

A campaign declares its beats as a record of overrides (for The Dunwich Legacy,
"Arkham.Narrative.TheDunwichLegacy.Types"), resolves whichever variant the game
was created with, and reads beats off it. So the campaign's own code names no
variant, and adding one touches only that campaign's registry:

> narrative <- getDunwichNarrative
> storyBuild =<< narrativeBuilder narrative.theHouseAlwaysWinsIntro (h "title" >> p "body")

* 'narrativeBuilder' replaces a flavor-only block (a scenario intro).
* 'narrativeResolution' replaces a resolution's flavor, keeping its mechanics.
* 'narrativeKey' refines which key a block renders, leaving its mechanics alone.
* 'onNarrative' runs extra narrative a variant adds.
-}
module Arkham.Helpers.Narrative where

import Arkham.Campaign.Option
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log (getCampaignOptions)
import Arkham.Helpers.Query (getLead)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Trait (Trait)
import Data.Set qualified as Set

{- | A beat whose flavor is replaced wholesale. The variant builds it in the
monad, because its branches read the campaign log and the Lead Investigator.
-}
type Flavor m = Maybe (m (FlavorTextBuilder ()))

-- | A beat whose locale key is chosen by game state, its mechanics left alone.
type FlavorKey m = Maybe (m Scope)

-- | Narrative a variant adds beside a beat the Campaign Guide already prints.
type Extra m = Maybe (m ())

{- | A block the Campaign Guide prints and the variant does not tell at all --
its scene is covered elsewhere in the rewrite.
-}
type Omit = Bool

{- | The narrative variant a campaign log was created with, if any.

The pure form, for 'Arkham.Campaign.Types.IsCampaign.nextStep', which has the
attrs but no game to read.
-}
narrativeOf :: Set CampaignOption -> Maybe Text
narrativeOf options = listToMaybe [v | NarrativeVariant v <- Set.toList options]

-- | The narrative variant this game was created with, if any.
getNarrative :: HasGame m => m (Maybe Text)
getNarrative = narrativeOf <$> getCampaignOptions

{- | The flavor a block renders: the narrative's, built in the monad because its
branches read game state, or the guide's as written.
-}
narrativeBuilder
  :: (HasI18n, Applicative m)
  => Maybe (m (FlavorTextBuilder ()))
  -> (HasI18n => FlavorTextBuilder ())
  -> m (FlavorTextBuilder ())
narrativeBuilder override base = fromMaybe (pure base) override

{- | A resolution whose flavor a narrative builds from several pieces, where the
guide renders one block.

The experience is resolved first and in scope while either flavor is built, so
@{xp}@ interpolates the same in both. Only the flavor differs -- the resolution's
records, trauma and tokens stay where they are.
-}
narrativeResolution
  :: (HasI18n, ReverseQueue m)
  => Scope
  -> Maybe (m (FlavorTextBuilder ()))
  -> m Int
  -> m ()
narrativeResolution base override gainXp = do
  xp <- gainXp
  withXp xp do
    builder <- narrativeBuilder override (setTitle (base <> ".title") >> p (base <> ".body"))
    resolutionFlavor builder

-- | Extra narrative a variant adds, read only when that variant is in play.
onNarrative :: Applicative m => Maybe (m ()) -> m ()
onNarrative = fromMaybe (pure ())

{- | The key a block renders under: the guide's, or the one the narrative in play
picks for the state the game is in. The block's mechanics are untouched, so this
is what a resolution a variant splits by trait or class uses.
-}
narrativeKey :: Applicative m => Scope -> Maybe (m Scope) -> m Scope
narrativeKey base = fromMaybe (pure base)

-- * The Lead Investigator

{- $lead
A rewrite that presupposes the Lead Investigator as the "you" of a scene frames
some of them around who that is. These answer the conditions such a rewrite
prints, in its own words.
-}

leadIs :: HasGame m => InvestigatorMatcher -> m Bool
leadIs matcher = do
  lead <- getLead
  lead <=~> matcher

leadHasTrait :: HasGame m => Trait -> m Bool
leadHasTrait = leadIs . InvestigatorWithTrait

leadIsClass :: HasGame m => ClassSymbol -> m Bool
leadIsClass = leadIs . InvestigatorWithClass

-- | By title, so a revised or parallel version of an investigator still counts.
leadIsNamed :: HasGame m => [Text] -> m Bool
leadIsNamed = anyM (leadIs . InvestigatorWithTitle)

-- | The first branch whose condition holds, else the rewrite's @otherwise@.
firstBranch :: Monad m => [(m Bool, Scope)] -> m Scope
firstBranch = go
 where
  go [] = pure "otherwise"
  go ((cond, key) : rest) = do
    matched <- cond
    if matched then pure key else go rest
