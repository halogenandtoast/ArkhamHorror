{- | Condition cards. A condition is double sided; 'backIsCondition' marks a
back that is itself a named condition you can gain, and 'hiddenBack' marks a
card you may not look at the back of until instructed.

The six dark pacts share one front and differ only on the back. Dead of Night's
six WANTED cards work the same way: one front, six different reckonings for what
finally catches up with you.
-}
module AH3e.Content.Conditions (cards, focusLimitBonuses) where

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
cards =
  core
    <> fromBox DeadOfNight deadOfNight
    <> fromBox UnderDarkWaves underDarkWaves
    <> fromBox SecretsOfTheOrder secretsOfTheOrder

{- | Conditions whose face-up side raises their holder's focus limit. Read where
the limit is worked out, which cannot see behaviours; the flipped side gives
nothing, which 'AH3e.Engine.Query.focusLimit' takes care of.
-}
focusLimitBonuses :: [(CardCode, Int)]
focusLimitBonuses = [("driven", 1)]

{- | Secrets of the Order's DRIVEN, whose back is the FATIGUED it costs you. The
drive buys an extra action at the end of a turn and the card turns over to pay
for it; fatigue takes a die for every reroll until a focus action shakes it off.
-}
secretsOfTheOrder :: [CardDef]
secretsOfTheOrder =
  [ condition
      "driven"
      "Driven / Fatigued"
      ( "DRIVEN"
      , "Your focus limit is increased by one. At the end of your turn, if you are not FATIGUED, you may flip this card to perform one additional action."
      )
      ( "FATIGUED"
      , "If you are DRIVEN, discard that card. You cannot become DRIVEN. While resolving a test, as an additional cost to reroll one or more dice, remove one die from that test. After you perform a focus action, discard this card."
      )
      True
      False
  ]

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

{- | Under Dark Waves' TAINTED cards: one front, and six unnamed backs for what
the corruption finally does to you. The backs are numbered rather than named, so
each card is told apart by its number.
-}
taintedFront :: (ConditionName, Text)
taintedFront =
  ( "TAINTED"
  , "After you draw a blank or spawn clue mythos token, place one doom in your space. Reckoning-Flip this card. (Do not look at the back of this card until you are instructed to do so.)"
  )

taintedCard :: Int -> Text -> CardDef
taintedCard n backText =
  condition
    (CardCode ("tainted-" <> tshow n))
    ("Tainted (" <> tshow n <> ")")
    taintedFront
    ("TAINTED", backText)
    False
    True

underDarkWaves :: [CardDef]
underDarkWaves =
  [ taintedCard
      1
      "Test will and resolve the effect based on your test result: 0: Choose another investigator in any space to suffer two damage and two horror. 1: Choose another investigator in any space to suffer one damage and one horror. 2+: No effect. Then discard this card."
  , taintedCard
      2
      "Test will and resolve the effect based on your test result: 0-1: You become CURSED. 2+: No effect. Then discard this card."
  , taintedCard
      3
      "Test will and resolve the effect based on your test result: 0: You suffer two damage and two horror. 1: You suffer one damage and one horror. 2+: No effect. Then discard this card."
  , taintedCard
      4
      "Test will and resolve the effect based on your test result: 0-1: You gain a DARK PACT. 2+: No effect. Then discard this card."
  , taintedCard
      5
      "Test will and resolve the effect based on your test result: 0: Spawn one monster in your space. If it engages an investigator, that monster attacks. 1: Spawn one monster in your space. 2+: No effect. Then discard this card."
  , taintedCard
      6
      "Test will and resolve the effect based on your test result: 0: You discard one talent. If you cannot, you discard all of your focus tokens. 1-2: You discard all of your focus tokens. 3+: No effect. Then discard this card."
  , darkPact
      "dark-pact-virulent-plague"
      "Virulent Plague"
      "You suffer one direct damage and one direct horror. Then each other investigator suffers two direct damage and two direct horror. Then discard this card."
  , darkPact
      "dark-pact-grim-spectre"
      "Grim Spectre"
      "Grim Spectre, a Phantom monster with two health, elite 1, the watcher keyword, a strength modifier of -2 and dealing two horror, cannot be evaded. When revealed, this monster engages you. You cannot evade or disengage this monster, and it cannot engage any other investigator."
  ]
