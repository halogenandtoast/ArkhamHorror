module Arkham.Ai.Decks (
  rolandCoreDeck,
  bundledDeckFor,
) where

import Arkham.Card.CardCode
import Arkham.Decklist.Type (ArkhamDBDecklist (..))
import Arkham.Prelude
import Data.Map.Strict qualified as Map

-- | A fixed, self-contained Roland Banks "Core Set" decklist (arkham.build deck
-- @20344@). An AI seat loads this directly via 'Arkham.Message.LoadDecklist', so
-- it never needs a saved deck or a deck-selection prompt.
--
-- The payload type is 'ArkhamDBDecklist' (what @LoadDecklist@ carries and what
-- the engine's @loadDecklist@ consumes), built exactly the way the engine
-- already constructs decklists programmatically (see
-- @DisappearanceAtTheTwilightEstate@). The @slots@ map mirrors the @slots@
-- object of an ArkhamDB decklist: code-to-quantity, with the signature asset
-- (@01006@) and signature weakness (@01007@) living in the slots alongside the
-- 30 deck cards, plus the random-basic-weakness placeholder @01000@. @01000@ is
-- a real 'CardDef' (@randomWeakness@), so it loads cleanly and is later
-- materialized into a concrete basic weakness at campaign/scenario setup
-- (@addRandomBasicWeaknessIfNeeded@), exactly as a real imported deck would be.
rolandCoreDeck :: ArkhamDBDecklist
rolandCoreDeck =
  ArkhamDBDecklist
    { slots = rolandCoreSlots
    , sideSlots = mempty
    , investigator_code = "01001"
    , investigator_name = "Roland Banks"
    , meta = Nothing
    , taboo_id = Nothing
    , url = Nothing
    , decklist_id = Just "20344"
    , decklist_name = Just "Roland Banks (Core Set)"
    }

-- | 30 deck cards + 3 fixed (signature, signature weakness, random basic
-- weakness). Every code is a valid entry in @allPlayerCards@.
rolandCoreSlots :: Map CardCode Int
rolandCoreSlots =
  Map.fromList
    [ ("01006", 1) -- Roland's .38 Special (signature)
    , ("01007", 1) -- Cover Up (signature weakness)
    , ("01000", 1) -- Random Basic Weakness (placeholder)
    , ("01016", 1) -- .45 Automatic
    , ("01017", 2) -- Physical Training
    , ("01020", 2) -- Machete
    , ("01021", 2) -- Guard Dog
    , ("01022", 2) -- Evidence!
    , ("01023", 2) -- Dodge
    , ("01024", 1) -- Dynamite Blast
    , ("01025", 2) -- Vicious Blow
    , ("01037", 2) -- Working a Hunch
    , ("01039", 2) -- Deduction
    , ("01086", 2) -- Knife
    , ("01087", 2) -- Flashlight
    , ("01088", 2) -- Emergency Cache
    , ("01089", 2) -- Guts
    , ("01091", 2) -- Overpower
    , ("01093", 2) -- Unexpected Courage
    ]

-- | The bundled decklist for a given investigator code, if one exists. Roland
-- Banks (@01001@) is the only profile shipped with the AI MVP; any other code
-- yields 'Nothing' (the seat then falls back to a normal deck prompt).
bundledDeckFor :: CardCode -> Maybe ArkhamDBDecklist
bundledDeckFor = \case
  "01001" -> Just rolandCoreDeck
  _ -> Nothing
