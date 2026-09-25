{- | Condition cards. A condition is double sided; 'backIsCondition' marks a
back that is itself a named condition you can gain, and 'hiddenBack' marks a
card you may not look at the back of until instructed.

The six dark pacts share one front and differ only on the back, as do the two
from later expansions whose backs are themselves conditions.
-}
module AH3e.Content.Conditions (cards) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect (ConditionName (..))
import AH3e.Types.Ids

condition
  :: CardCode -> Text -> (ConditionName, Text) -> (ConditionName, Text) -> Bool -> Bool -> CardDef
condition code name (frontName, frontText) (backName, backText) backIsCondition hiddenBack =
  CardDef
    code
    name
    CoreSet
    1
    ( ConditionCard
        ConditionDef
          { front = ConditionFace frontName frontText
          , back = ConditionFace backName backText
          , backIsCondition
          , hiddenBack
          }
    )

darkPactFront :: (ConditionName, Text)
darkPactFront =
  ( "DARK PACT"
  , "Reckoning-Roll one die. On a 1, your debt has come due; flip this card. (This die roll is not a test.) (Do not look at the back of this card until you are instructed to do so.)"
  )

darkPact :: CardCode -> Text -> Text -> CardDef
darkPact code backName backText =
  condition
    code
    ("Dark Pact (" <> backName <> ")")
    darkPactFront
    (ConditionName backName, backText)
    False
    True

cards :: [CardDef]
cards =
  [ condition
      "blessed"
      "Blessed / Cursed"
      ( "BLESSED"
      , "While resolving a test, 4s, 5s, and 6s count as successes. After you fail a test, discard this card. If you would become CURSED, discard this card instead."
      )
      ( "CURSED"
      , "While resolving a test, only 6s count as successes. After you pass a test, discard this card. If you would become BLESSED, discard this card instead."
      )
      True
      False
  , darkPact
      "dark-pact-the-world-undone"
      "The World Undone"
      "Place three doom in your space. Then you discard this card."
  , darkPact
      "dark-pact-pact-of-sacrifice"
      "Pact of Sacrifice"
      "Choose another investigator on any space. That investigator is devoured. Then you discard this card."
  , darkPact
      "dark-pact-the-ultimate-price"
      "The Ultimate Price"
      "You are devoured."
  , darkPact
      "dark-pact-an-alliance-of-evil"
      "An Alliance of Evil"
      "Spawn one monster in each space in your neighborhood. (Each monster engages an investigator in its space.) Each monster recovers all of its health and deals damage and horror to the investigator it has engaged. Then you discard this card."
  , darkPact
      "dark-pact-dark-destiny"
      "Dark Destiny"
      "You draw and resolve six tokens from the mythos cup. Then you discard this card."
  , darkPact
      "dark-pact-forbidden-knowledge"
      "Forbidden Knowledge"
      "Discard three clues total from among all investigators and the scenario sheet (or all such clues if there are fewer than three). If exactly zero or one clue is discarded this way, place one doom on the scenario sheet. Then you discard this card."
  , -- these two backs are themselves conditions you can gain by name
    condition
      "dark-pact-tainted"
      "Dark Pact (Tainted)"
      darkPactFront
      ( "TAINTED"
      , "After you draw a blank or spawn clue mythos token, place one doom in your space. Reckoning-Flip this card. (Do not look at the back of this card until you are instructed to do so.)"
      )
      True
      True
  , condition
      "dark-pact-wanted"
      "Dark Pact (Wanted)"
      darkPactFront
      ( "WANTED"
      , "Discard any reputation talents. You may not gain reputation talents. Reckoning-Test influence. If you fail, flip this card. If your test result was two or greater, discard this card."
      )
      True
      True
  ]
