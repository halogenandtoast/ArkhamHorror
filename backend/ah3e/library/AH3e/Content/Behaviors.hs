module AH3e.Content.Behaviors (behaviors) where

import AH3e.Content.AllyBehaviors qualified as Allies
import AH3e.Content.ConditionBehaviors qualified as Conditions
import AH3e.Content.Core.ApproachOfAzathothBehaviors qualified as ApproachOfAzathoth
import AH3e.Content.Core.FeastOfUmordhothBehaviors qualified as FeastOfUmordhoth
import AH3e.Content.Core.InvestigatorBehaviors qualified as Investigators
import AH3e.Content.HeadlineBehaviors qualified as Headlines
import AH3e.Content.ItemBehaviors qualified as Items
import AH3e.Content.SpecialBehaviors qualified as Specials
import AH3e.Content.SpellBehaviors qualified as Spells
import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Prelude
import AH3e.Types.Card (MythosToken (..))
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  ApproachOfAzathoth.behaviors
    <> Investigators.behaviors
    <> FeastOfUmordhoth.behaviors
    <> Conditions.behaviors
    <> Allies.behaviors
    <> Headlines.behaviors
    <> Items.behaviors
    <> Specials.behaviors
    <> Spells.behaviors
    <> mempty
      { customEffects =
          Map.fromList
            [ ("clover-club-craps", cloverClubCraps)
            , ("return-spawn-monster-token", returnSpawnMonsterToken)
            ]
      }

-- rule 474: a roll outside a test, so nothing can reroll or modify it
cloverClubCraps :: EffectCtx -> GameM ()
cloverClubCraps ctx = do
  a <- rollDie
  b <- rollDie
  let total = a + b
  logText ("Rolled " <> tshow a <> " and " <> tshow b <> " (" <> tshow total <> ")")
  if
    | total `elem` [7, 11] -> addMoney ctx.investigator 5
    | total `elem` [2, 3, 12] -> addMoney ctx.investigator (-3)
    | otherwise -> pure ()

-- Cerebral Extractor: return a spawn monster token to the mythos cup
returnSpawnMonsterToken :: EffectCtx -> GameM ()
returnSpawnMonsterToken _ = do
  drawn <- use #drawnTokens
  case break (== SpawnMonsterToken) drawn of
    (before, _ : after) -> do
      #drawnTokens .= before <> after
      #cup %= (SpawnMonsterToken :)
      logText "A spawn monster token returns to the mythos cup"
    _ -> logText "No spawn monster token to return to the mythos cup"
