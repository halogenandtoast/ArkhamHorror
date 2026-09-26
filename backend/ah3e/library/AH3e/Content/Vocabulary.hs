-- | Shared shorthand for writing encounter effects.
module AH3e.Content.Vocabulary where

import AH3e.Prelude
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Text qualified as T

spell, ally, commonItem, curioItem, tomeItem, blessed, cursed, delayed :: Effect
spell = GainE (ASpell Nothing)
ally = GainE (AnAlly Nothing)
commonItem = GainE (AnItem (Just "Common"))
curioItem = GainE (AnItem (Just "Curio"))
tomeItem = GainE (AnItem (Just "Tome"))
blessed = GainE (Condition "BLESSED")
cursed = GainE (Condition "CURSED")
delayed = BecomeDelayed

money, remnants, sanity, health, horror, damage, mySanity, myHealth :: Int -> Effect
money n = GainE (Money (N n))
remnants n = GainE (Remnants (N n))
sanity n = RecoverSanity YouOrAlly (N n)
health n = RecoverHealth YouOrAlly (N n)
mySanity n = RecoverSanity You (N n)
myHealth n = RecoverHealth You (N n)
horror n = SufferHorror (N n)
damage n = SufferDamage (N n)

harm :: Int -> Int -> Effect
harm d h = SufferHarmE (N d) (N h)

named :: Text -> Effect
named = GainE . Named

pass :: Skill -> Int -> Effect -> Effect
pass sk m e = Test sk m e NoEffect

mayPay :: Cost -> Effect -> Effect
mayPay c e = MayPay c e NoEffect

orPay :: Skill -> Text -> Cost -> Effect -> Effect
orPay sk costLabel cost e = Choose [("Test " <> skillText sk, pass sk 0 e), (costLabel, Pay cost e)]

skillText :: Skill -> Text
skillText = T.toLower . tshow

focusAny, focusExceed :: Effect
focusAny = Focus Nothing False
focusExceed = Focus Nothing True

buyAny, buyOne, buyOneHalf :: Trait -> Effect
buyAny t = BuyFromDisplay (Just t) False Nothing NoEffect
buyOne t = BuyFromDisplay (Just t) False (Just 1) NoEffect
buyOneHalf t = BuyFromDisplay (Just t) True (Just 1) NoEffect

spells :: Int -> Maybe Int -> Pricing -> Effect
spells = BuyFromDeck SpellDeckKind
