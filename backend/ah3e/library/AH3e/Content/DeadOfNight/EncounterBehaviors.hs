-- | The Dead of Night encounters whose text the effect vocabulary cannot say.
module AH3e.Content.DeadOfNight.EncounterBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("don-lucky-charm", luckyCharm)
          , ("don-research-one", researchOne)
          , ("don-ancestral-memory", ancestralMemory)
          , ("don-cycle-display-2", cycleDisplay)
          , ("don-remote-trade", remoteTrade)
          , ("don-slip-away", slipAway)
          , ("don-maeve-chapman", maeveChapman)
          , ("don-fireworks", fireworks)
          ]
    }

-- rule 474: a roll outside a test, so nothing can reroll or modify it
luckyCharm :: EffectCtx -> GameM ()
luckyCharm ctx = do
  v <- rollDie
  logText ("Rolled " <> tshow v)
  addMoney ctx.investigator v

researchOne :: EffectCtx -> GameM ()
researchOne ctx = push (ResearchClues ctx.investigator 1)

{- | "For each success you roll, reveal one spell from the deck; if you reveal one
or more cards, gain one of those revealed spells." The reveal is free, so it is a
purchase at no price.
-}
ancestralMemory :: EffectCtx -> GameM ()
ancestralMemory ctx = case ctx.testResult of
  Just n | n > 0 -> push (ResolveEffect ctx (BuyFromDeck SpellDeckKind n (Just 1) (FlatPrice 0)))
  _ -> pure ()

cycleDisplay :: EffectCtx -> GameM ()
cycleDisplay ctx = push (CycleDisplay ctx.investigator 2)

-- | "Trade with an investigator in any space as though you were in that space."
remoteTrade :: EffectCtx -> GameM ()
remoteTrade ctx = do
  others <- filter ((/= ctx.investigator) . (.id)) <$> playingInvestigators
  unless (null others)
    $ chooseFor
      ctx.investigator
      "Trade with an investigator in any space"
      ( Choice (DoneLabel "Decline") []
          : [Choice (InvestigatorLabel i.id) [TradeWith ctx.investigator i.id] | i <- others]
      )

-- | "You may disengage all monsters and move up to two spaces."
slipAway :: EffectCtx -> GameM ()
slipAway ctx = do
  ms <- uses #monsters (filter (isEngagedWith ctx.investigator) . Map.elems)
  pushAll
    $ [DisengageMonster ctx.investigator m.card | m <- ms]
    <> [ResolveEffect ctx (MoveUpTo 2)]

-- | Nurse Chapman only comes along if you are actually hurt.
maeveChapman :: EffectCtx -> GameM ()
maeveChapman ctx = do
  i <- getInvestigator ctx.investigator
  when (i.damage >= 1) $ push (GainNamedCard ctx.investigator "MAEVE CHAPMAN")

-- | "Exhaust one monster in any space. (It disengages any investigators.)"
fireworks :: EffectCtx -> GameM ()
fireworks ctx = do
  ms <- uses #monsters Map.elems
  let ready = [m | m <- ms, m.state /= Exhausted]
  unless (null ready)
    $ chooseFor
      ctx.investigator
      "Exhaust one monster"
      [Choice (MonsterLabel m.card) [ExhaustMonster m.card] | m <- ready]
