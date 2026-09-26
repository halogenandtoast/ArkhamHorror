-- | Mechanics for allies whose text the effect vocabulary cannot express.
module AH3e.Content.AllyBehaviors (behaviors) where

import AH3e.Content.Vocabulary (curioItem)
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
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
