module Arkham.Effect.Effects.LuckyDice2 (
  LuckyDice2 (..),
  luckyDice2,
) where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers

newtype LuckyDice2 = LuckyDice2 (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

newtype Metadata = Metadata {alreadyTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks)

luckyDice2 :: EffectArgs -> LuckyDice2
luckyDice2 = LuckyDice2 . (`with` Metadata False) . uncurry4 (baseAttrs "02230")

instance HasModifiersFor LuckyDice2 where
  getModifiersFor target (LuckyDice2 (attrs `With` _)) | target == effectTarget attrs = do
    pure $ toModifiers attrs [IgnoreChaosToken]
  getModifiersFor _ _ = pure []

instance RunMessage LuckyDice2 where
  runMessage msg e@(LuckyDice2 (attrs@EffectAttrs {..} `With` (Metadata hasDrawn))) =
    case msg of
      RevealChaosToken _ _ token -> do
        when (not hasDrawn && chaosTokenFace token == AutoFail) $ do
          case effectSource of
            AssetSource aid -> push (RemoveFromGame $ AssetTarget aid)
            _ -> error "wrong source"
        pure $ LuckyDice2 $ attrs `with` Metadata True
      SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
      _ -> LuckyDice2 . (`with` Metadata hasDrawn) <$> runMessage msg attrs
