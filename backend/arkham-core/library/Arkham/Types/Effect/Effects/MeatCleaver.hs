module Arkham.Types.Effect.Effects.MeatCleaver
  ( MeatCleaver(..)
  , meatCleaver
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Message

newtype MeatCleaver = MeatCleaver EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meatCleaver :: EffectArgs -> MeatCleaver
meatCleaver = MeatCleaver . uncurry4 (baseAttrs "05114")

instance HasQueue env => RunMessage env MeatCleaver where
  runMessage msg e@(MeatCleaver attrs) = case msg of
    EnemyDefeated _ _ _ _ source _ | effectSource attrs == source ->
      e <$ pushAll
        [HealHorror (effectTarget attrs) 1, DisableEffect $ toId attrs]
    SkillTestEnds _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> MeatCleaver <$> runMessage msg attrs
