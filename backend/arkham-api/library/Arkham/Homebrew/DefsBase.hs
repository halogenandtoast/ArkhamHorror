{-# LANGUAGE AllowAmbiguousTypes #-}

module Arkham.Homebrew.DefsBase where

import Arkham.Action (Action)
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Criteria (Criterion)
import Arkham.Prelude
import Arkham.Trait (Trait)

{- | Everything a homebrew campaign (or standalone) contributes: its card
definition tables (one slot per @all*Cards@ aggregate) and its trait values.
-}
data HomebrewDefs = HomebrewDefs
  { hdLocations :: [CardDef]
  , hdEnemies :: [CardDef]
  , hdTreacheries :: [CardDef]
  , hdPlayerTreacheries :: [CardDef]
  , hdActs :: [CardDef]
  , hdAgendas :: [CardDef]
  , hdEncounterAssets :: [CardDef]
  , hdPlayerAssets :: [CardDef]
  {- ^ Assets with a *player* card back: they live in investigator decks, so
  they are folded into @allPlayerAssetCards@ rather than the encounter map.
  -}
  , hdPlayerSkills :: [CardDef]
  , hdStories :: [CardDef]
  , hdPlayerStories :: [CardDef]
  {- ^ Story cards printed on a player card back (Dark Matter's "Delights").
  They must resolve both as a story entity and as a deck-legal player card,
  so they are folded into @allStoryCards@ *and* @allPlayerCards@.
  -}
  , hdTraits :: [Trait]
  {- ^ The campaign's own trait values (see its @Traits.hs@). Folded into
  @Arkham.Homebrew.Defs.allTraits@ so they appear wherever the full trait
  universe is enumerated.
  -}
  , hdActions :: [Action]
  {- ^ The campaign's own action values (see its @Actions.hs@). Folded into
  @Arkham.Homebrew.Defs.allActions@.
  -}
  , hdActionAffordability :: [(Action, Criterion)]
  {- ^ Per-action affordability rules for this campaign's actions, evaluated
  generically by core (see @Arkham.Homebrew.Defs.homebrewActionAffordability@
  and its use in @Arkham.Helpers.Ability.canDoAction'@). An action with no
  entry is always affordable.
  -}
  }

instance Semigroup HomebrewDefs where
  a <> b =
    HomebrewDefs
      { hdLocations = hdLocations a <> hdLocations b
      , hdEnemies = hdEnemies a <> hdEnemies b
      , hdTreacheries = hdTreacheries a <> hdTreacheries b
      , hdPlayerTreacheries = hdPlayerTreacheries a <> hdPlayerTreacheries b
      , hdActs = hdActs a <> hdActs b
      , hdAgendas = hdAgendas a <> hdAgendas b
      , hdEncounterAssets = hdEncounterAssets a <> hdEncounterAssets b
      , hdPlayerAssets = hdPlayerAssets a <> hdPlayerAssets b
      , hdPlayerSkills = hdPlayerSkills a <> hdPlayerSkills b
      , hdStories = hdStories a <> hdStories b
      , hdPlayerStories = hdPlayerStories a <> hdPlayerStories b
      , hdTraits = hdTraits a <> hdTraits b
      , hdActions = hdActions a <> hdActions b
      , hdActionAffordability = hdActionAffordability a <> hdActionAffordability b
      }

instance Monoid HomebrewDefs where
  mempty = HomebrewDefs [] [] [] [] [] [] [] [] [] [] [] [] [] []

{- | Implement in your campaign's @Defs.hs@ on a campaign-local tag type; the
instance is discovered automatically (see 'Arkham.Homebrew.Defs').
-}
class IsHomebrewDefs a where
  homebrewDefs :: HomebrewDefs

{- | A 'CardDef' printed on a *player* card back whose card type cannot say so.
The alias is transparent, so it changes nothing about the definition itself; it
is the signature homebrew card-def discovery reads to route a def into the
player-side slot of 'HomebrewDefs'.

Only story cards need it (Dark Matter's "Delights"): assets carry the same fact
in their card type — 'AssetType' for a player back, 'EncounterAssetType' for an
encounter back, which is what @encounterAsset@ builds.
-}
type PlayerCardDef = CardDef

-- | A discovered card definition together with the card back it was declared with.
data HomebrewCardDef = HomebrewCardDef
  { hcdPlayerBack :: Bool
  , hcdDef :: CardDef
  }

-- | Registration helpers used by generated homebrew card-def entry modules.
cardDefEntry :: CardDef -> [HomebrewCardDef]
cardDefEntry def = [HomebrewCardDef False def]

playerCardDefEntry :: PlayerCardDef -> [HomebrewCardDef]
playerCardDefEntry def = [HomebrewCardDef True def]

{- | Implemented by the generated @CardDefEntries.hs@ of each campaign, which
collects every @<name> :: CardDef@ under its @CardDefs/@ directory; the
instance is spliced back in by @generateHomebrewCardDefs@ in that campaign's
@Defs.hs@.
-}
class IsHomebrewCardDefs a where
  homebrewCardDefs :: [HomebrewCardDef]

{- | Sort discovered card definitions into the 'HomebrewDefs' slots. Every slot
but the player/encounter split follows from 'cdCardType'; the split follows
from whether the def was declared as a 'PlayerCardDef'.
-}
discoveredDefs :: [HomebrewCardDef] -> HomebrewDefs
discoveredDefs = foldMap toDefs
 where
  toDefs (HomebrewCardDef playerBack def) = case cdCardType def of
    LocationType -> mempty {hdLocations = [def]}
    EnemyType -> mempty {hdEnemies = [def]}
    TreacheryType -> mempty {hdTreacheries = [def]}
    PlayerTreacheryType -> mempty {hdPlayerTreacheries = [def]}
    ActType -> mempty {hdActs = [def]}
    AgendaType -> mempty {hdAgendas = [def]}
    EncounterAssetType -> mempty {hdEncounterAssets = [def]}
    AssetType -> mempty {hdPlayerAssets = [def]}
    SkillType -> mempty {hdPlayerSkills = [def]}
    StoryType
      | playerBack -> mempty {hdPlayerStories = [def]}
      | otherwise -> mempty {hdStories = [def]}
    cardType ->
      error
        $ "Arkham.Homebrew.DefsBase.discoveredDefs: no homebrew slot for "
        <> show cardType
        <> " ("
        <> show (cdCardCode def)
        <> "); add one to HomebrewDefs and fold it into the matching all*Cards aggregate"
