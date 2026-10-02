module AH3e.Content.Behaviors (behaviors) where

import AH3e.Content.AllyBehaviors qualified as Allies
import AH3e.Content.ConditionBehaviors qualified as Conditions
import AH3e.Content.Core.ApproachOfAzathothBehaviors qualified as ApproachOfAzathoth
import AH3e.Content.Core.EchoesOfTheDeepBehaviors qualified as EchoesOfTheDeep
import AH3e.Content.Core.FeastOfUmordhothBehaviors qualified as FeastOfUmordhoth
import AH3e.Content.Core.InvestigatorBehaviors qualified as Investigators
import AH3e.Content.Core.VeilOfTwilightBehaviors qualified as VeilOfTwilight
import AH3e.Content.DeadOfNight.EncounterBehaviors qualified as DeadOfNightEncounters
import AH3e.Content.DeadOfNight.InvestigatorBehaviors qualified as DeadOfNightInvestigators
import AH3e.Content.DeadOfNight.ShotsInTheDarkBehaviors qualified as ShotsInTheDark
import AH3e.Content.DeadOfNight.SilenceOfTsathogguaBehaviors qualified as SilenceOfTsathoggua
import AH3e.Content.DeadOfNight.StartingBehaviors qualified as DeadOfNightStarting
import AH3e.Content.HeadlineBehaviors qualified as Headlines
import AH3e.Content.ItemBehaviors qualified as Items
import AH3e.Content.MonsterBehaviors qualified as Monsters
import AH3e.Content.SecretsOfTheOrder.BoundToServeBehaviors qualified as BoundToServe
import AH3e.Content.SecretsOfTheOrder.EncounterBehaviors qualified as SecretsOfTheOrderEncounters
import AH3e.Content.SecretsOfTheOrder.InvestigatorBehaviors qualified as SecretsOfTheOrderInvestigators
import AH3e.Content.SecretsOfTheOrder.StartingBehaviors qualified as SecretsOfTheOrderStarting
import AH3e.Content.SecretsOfTheOrder.TheDeadCryOutBehaviors qualified as TheDeadCryOut
import AH3e.Content.SecretsOfTheOrder.TheKeyAndTheGateBehaviors qualified as TheKeyAndTheGate
import AH3e.Content.SpecialBehaviors qualified as Specials
import AH3e.Content.SpellBehaviors qualified as Spells
import AH3e.Content.UnderDarkWaves.DreamsOfRlyehBehaviors qualified as DreamsOfRlyeh
import AH3e.Content.UnderDarkWaves.InvestigatorBehaviors qualified as UnderDarkWavesInvestigators
import AH3e.Content.UnderDarkWaves.IthaquasChildrenBehaviors qualified as IthaquasChildren
import AH3e.Content.UnderDarkWaves.StartingBehaviors qualified as UnderDarkWavesStarting
import AH3e.Content.UnderDarkWaves.TerrorBehaviors qualified as UnderDarkWavesTerrors
import AH3e.Content.UnderDarkWaves.ThePaleLanternBehaviors qualified as ThePaleLantern
import AH3e.Content.UnderDarkWaves.TyrantsOfRuinBehaviors qualified as TyrantsOfRuin
import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card (MythosToken (..))
import AH3e.Types.Effect
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  ApproachOfAzathoth.behaviors
    <> Investigators.behaviors
    <> FeastOfUmordhoth.behaviors
    <> EchoesOfTheDeep.behaviors
    <> VeilOfTwilight.behaviors
    <> SilenceOfTsathoggua.behaviors
    <> ShotsInTheDark.behaviors
    <> DeadOfNightStarting.behaviors
    <> DeadOfNightEncounters.behaviors
    <> DeadOfNightInvestigators.behaviors
    <> UnderDarkWavesInvestigators.behaviors
    <> UnderDarkWavesStarting.behaviors
    <> SecretsOfTheOrderInvestigators.behaviors
    <> SecretsOfTheOrderStarting.behaviors
    <> SecretsOfTheOrderEncounters.behaviors
    <> BoundToServe.behaviors
    <> TheKeyAndTheGate.behaviors
    <> TheDeadCryOut.behaviors
    <> UnderDarkWavesTerrors.behaviors
    <> TyrantsOfRuin.behaviors
    <> IthaquasChildren.behaviors
    <> DreamsOfRlyeh.behaviors
    <> ThePaleLantern.behaviors
    <> Conditions.behaviors
    <> Allies.behaviors
    <> Headlines.behaviors
    <> Items.behaviors
    <> Monsters.behaviors
    <> Specials.behaviors
    <> Spells.behaviors
    <> mempty
      { customEffects =
          Map.fromList
            [ ("clover-club-craps", cloverClubCraps)
            , ("discard-source", discardSource)
            , ("recover-all", recoverAll)
            , ("spawn-or-research-clue", spawnOrResearchClue)
            , ("another-investigator-moves", investigatorMoves False)
            , ("any-investigator-moves", investigatorMoves True)
            , ("round-of-drinks", roundOfDrinks)
            , ("travel-onward", travelOnward Nothing)
            , ("travel-onward:beast", travelOnward (Just beastOnTheTracks))
            , ("travel-onward:stranger", travelOnward (Just (Test Influence 0 (GainE (AnAlly Nothing)) NoEffect)))
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
      returnTokensToCup [SpawnMonsterToken]
      logText "A spawn monster token returns to the mythos cup"
    _ -> logText "No spawn monster token to return to the mythos cup"

-- | "Recover all of your health and sanity", which no amount can name.
recoverAll :: EffectCtx -> GameM ()
recoverAll ctx = do
  i <- getInvestigator ctx.investigator
  push (RecoverInvestigator ctx.investigator i.damage i.horror)

{- | "Spawn or research one clue." Researching moves one of your own clues onto
the scenario sheet, so it is only offered to someone holding one.
-}
spawnOrResearchClue :: EffectCtx -> GameM ()
spawnOrResearchClue ctx = do
  i <- getInvestigator ctx.investigator
  chooseFor ctx.investigator "Spawn or research one clue"
    $ label "Spawn one clue" [SpawnClue]
    : [label "Research one clue" [ResearchCluesExact ctx.investigator 1] | i.clues > 0]

{- | "Another investigator may move one space", and the variant that lets the
reader move themselves. Whoever is reading picks who goes, and that investigator
chooses where; declining is what makes it a may.
-}
investigatorMoves :: Bool -> EffectCtx -> GameM ()
investigatorMoves includeSelf ctx = do
  invs <- playingInvestigators
  let movers = [o | o <- invs, includeSelf || o.id /= ctx.investigator]
  unless (null movers)
    $ chooseFor ctx.investigator "Choose an investigator to move one space"
    $ Choice (DoneLabel "Nobody moves") []
    : [ Choice (InvestigatorLabel o.id) [ResolveEffect (ctx & #investigator .~ o.id) (MoveUpTo 1)]
      | o <- movers
      ]

{- | "You may spend $1 for each ally and investigator in your space to recover one
sanity." The price is per head, so the dollars are capped at the number of them
who have any horror to lose; each one buys a single sanity.
-}
roundOfDrinks :: EffectCtx -> GameM ()
roundOfDrinks ctx = do
  (invs, allies) <- recoverTargets ctx InvestigatorOrAllyInYourSpace 0 1
  push (ResolveEffect ctx (rounds (length invs + length allies)))
 where
  rounds 0 = NoEffect
  rounds k =
    MayPay
      (SpendMoney 1)
      (Seq [RecoverSanity InvestigatorOrAllyInYourSpace (N 1), rounds (k - 1)])
      NoEffect

{- | "You may move one space or move to another <route>", which every travel route
encounter offers. The second option is a direct move to another travel route of
the same type, so it is only offered where there is one to go to. A card that
says "if you do" hands over an effect, which rides on the options that move.
-}
travelOnward :: Maybe Effect -> EffectCtx -> GameM ()
travelOnward after ctx = do
  msid <- investigatorSpace ctx.investigator
  board <- use #board
  offerOnward ctx "Travel onward?" True (maybe [] (`sameRouteSpaces` board) msid) after

-- | "The train hits a large beast on the way", for the one card that tests on arrival.
beastOnTheTracks :: Effect
beastOnTheTracks = Test Strength 0 (GainE (Remnants (N 1))) (SufferDamage (N 1))

-- | Discards whichever card is resolving the effect, for a card that spends itself.
discardSource :: EffectCtx -> GameM ()
discardSource ctx = case ctx.source of
  SourceCard cid -> push (DiscardAsset cid)
  _ -> pure ()
