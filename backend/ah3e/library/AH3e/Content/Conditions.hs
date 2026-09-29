{- | Condition cards. A condition is double sided; 'backIsCondition' marks a
back that is itself a named condition you can gain, and 'hiddenBack' marks a
card you may not look at the back of until instructed.

The six dark pacts share one front and differ only on the back. Dead of Night's
six WANTED cards work the same way: one front, six different reckonings for what
finally catches up with you.
-}
module AH3e.Content.Conditions (cards) where

import AH3e.Content.Vocabulary (fromBox)
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

core :: [CardDef]
core =
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
  ]

cards :: [CardDef]
cards = core <> fromBox DeadOfNight deadOfNight

wantedFront :: (ConditionName, Text)
wantedFront =
  ( "WANTED"
  , "Discard any reputation talents. You may not gain reputation talents. Reckoning-Test influence. If you fail, flip this card. If your test result was two or greater, discard this card."
  )

wantedCard :: CardCode -> Text -> Text -> CardDef
wantedCard code backName backText =
  condition
    code
    ("Wanted (" <> backName <> ")")
    wantedFront
    (ConditionName backName, backText)
    False
    True

deadOfNight :: [CardDef]
deadOfNight =
  [ wantedCard "wanted-beaten" "BEATEN" "You suffer two damage. Then you discard this card."
  , wantedCard "wanted-detained" "DETAINED" "You become delayed. Then you discard this card."
  , wantedCard
      "wanted-disarmed"
      "DISARMED"
      "You discard one non-curio weapon. Then you discard this card."
  , wantedCard
      "wanted-rattled"
      "RATTLED"
      "Choose one skill. During tests that use that skill, roll one fewer die. (Place a focus token that corresponds to that skill on this card as a reminder.) Reckoning-Discard this card. Do not resolve this effect during the phase this card was flipped."
  , wantedCard
      "wanted-shaken-down"
      "SHAKEN DOWN"
      "You discard all of your money. Then you discard this card."
  , wantedCard
      "wanted-vengeful-pursuer"
      "VENGEFUL PURSUER"
      "Vengeful Pursuer, a Human monster with three health, a strength modifier of 0 and an evade modifier of -2, dealing two damage. When revealed, this monster engages you. If this monster is not engaged with an investigator, discard it."
  ]
