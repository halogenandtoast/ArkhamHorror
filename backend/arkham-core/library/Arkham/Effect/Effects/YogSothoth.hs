module Arkham.Effect.Effects.YogSothoth
  ( YogSothoth(..)
  , yogSothoth
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message

newtype YogSothoth = YogSothoth EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EffectArgs -> YogSothoth
yogSothoth = YogSothoth . uncurry4 (baseAttrs "02323")

instance HasModifiersFor YogSothoth where
  getModifiersFor target (YogSothoth attrs) = case effectMetadata attrs of
    Just (EffectInt n) -> case target of
      EnemyTarget eid -> case effectSource attrs of
        EnemySource eid' | eid' == eid ->
          pure $ toModifiers attrs [HorrorDealt (-n)]
        _ -> pure []
      _ -> pure []
    _ -> pure []

instance RunMessage YogSothoth where
  runMessage msg e@(YogSothoth attrs) = case msg of
    DeckHasNoCards iid _ | isTarget attrs (InvestigatorTarget iid) ->
      e <$ push (DrivenInsane iid)
    _ -> YogSothoth <$> runMessage msg attrs
