-- | Mechanics for allies whose text the effect vocabulary cannot express.
module AH3e.Content.AllyBehaviors (behaviors) where

import AH3e.Content.Vocabulary (curioItem)
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #assets
    .~ Map.fromList
      [ ("alice-luxley", defaultAssetBehavior & #bonusDicePerRound .~ aliceLuxleyDice)
      , ("arthur-johnson", defaultAssetBehavior & #freeRerollPerRound .~ True)
      ,
        ( "delphinia-bell"
        , cardAction
            "Delphinia Bell: spend a remnant to focus a skill"
            (Pay (SpendRemnants 1) (Focus Nothing True))
        )
      ,
        ( "ezra-graves"
        , cardAction
            "Ezra Graves: suffer one direct horror to gain an ally"
            (Pay (SpendRemnants 2) (Seq [DirectHorror (N 1), GainE (AnAlly Nothing)]))
        )
      , ("gabriel-carillo", defaultAssetBehavior & #extraActions .~ 1)
      , ("grace-bechman", testBonuses [OnAction WardAction Lore 2])
      , ("leland-williams", defaultAssetBehavior & #afterGainedFromDeck ?~ curioItem)
      , ("lewis-hayes", testBonuses [WhileCasting 2])
      , ("sachiko-higa", testBonuses [OnAction AttackAction Strength 2])
      ,
        ( "zora-larson"
        , cardAction "Zora Larson: recover one sanity" (RecoverSanity InvestigatorOrAllyInYourSpace (N 1))
        )
      ]

{- | An "Action:" printed on a card: it spends an action like any other, is kept
back when it could accomplish nothing or its cost cannot be paid, and resolves
as the card's own effect.
-}
cardAction :: Text -> Effect -> AssetBehavior
cardAction lbl eff =
  defaultAssetBehavior
    & #componentActions
    .~ [ ComponentActionDef
           { label = lbl
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               let (cost, body) = case eff of
                     Pay c rest -> (Just c, rest)
                     _ -> (Nothing, eff)
               affordable <- maybe (pure True) (canPayCost iid) cost
               useful <- effectUseful (EffectCtx iid (SourceInvestigator iid) Nothing) body
               pure (affordable && useful)
           , perform = \ctx -> push (ResolveEffect ctx eff)
           }
       ]

-- | One die per clue you hold, plus one per clue in your neighborhood.
aliceLuxleyDice :: CardId -> InvestigatorId -> GameM Int
aliceLuxleyDice _ iid = do
  i <- getInvestigator iid
  here <- investigatorNeighborhood iid >>= maybe (pure 0) (fmap (.clues) . getNeighborhood)
  pure (i.clues + here)
