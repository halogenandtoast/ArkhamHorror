-- | Mechanics for allies whose text the effect vocabulary cannot express.
module AH3e.Content.AllyBehaviors (behaviors) where

import AH3e.Content.Vocabulary (curioItem)
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Ids
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #assets
    .~ Map.fromList
      [ ("alice-luxley", defaultAssetBehavior & #bonusDicePerRound .~ aliceLuxleyDice)
      , ("leland-williams", defaultAssetBehavior & #afterGainedFromDeck ?~ curioItem)
      , ("gabriel-carillo", defaultAssetBehavior & #extraActions .~ 1)
      ]

-- | One die per clue you hold, plus one per clue in your neighborhood.
aliceLuxleyDice :: CardId -> InvestigatorId -> GameM Int
aliceLuxleyDice _ iid = do
  i <- getInvestigator iid
  here <- investigatorNeighborhood iid >>= maybe (pure 0) (fmap (.clues) . getNeighborhood)
  pure (i.clues + here)
