module Arkham.Types.Effect.Effects.LuckyDice2
  ( LuckyDice2(..)
  , luckyDice2
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token

newtype LuckyDice2 = LuckyDice2 (EffectAttrs `With` Metadata)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newtype Metadata = Metadata { alreadyTriggered :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

luckyDice2 :: EffectArgs -> LuckyDice2
luckyDice2 =
  LuckyDice2 . (`with` Metadata False) . uncurry4 (baseAttrs "02230")

instance HasModifiersFor env LuckyDice2 where
  getModifiersFor _ target (LuckyDice2 (attrs `With` _))
    | target == effectTarget attrs = pure
    $ toModifiers attrs [IgnoreTokenEffects]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env LuckyDice2 where
  runMessage msg e@(LuckyDice2 (attrs@EffectAttrs {..} `With` (Metadata hasDrawn)))
    = case msg of
      RevealToken _ _ token -> do
        when
          (not hasDrawn && tokenFace token == AutoFail)
          (case effectSource of
            AssetSource aid -> push (RemoveFromGame $ AssetTarget aid)
            _ -> error "wrong source"
          )
        pure $ LuckyDice2 $ attrs `with` Metadata True
      SkillTestEnds _ -> e <$ push (DisableEffect effectId)
      _ -> LuckyDice2 . (`with` Metadata hasDrawn) <$> runMessage msg attrs
