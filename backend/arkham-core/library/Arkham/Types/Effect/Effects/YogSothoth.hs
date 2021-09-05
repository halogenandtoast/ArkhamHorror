module Arkham.Types.Effect.Effects.YogSothoth
  ( YogSothoth(..)
  , yogSothoth
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target

newtype YogSothoth = YogSothoth EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EffectArgs -> YogSothoth
yogSothoth = YogSothoth . uncurry4 (baseAttrs "02323")

instance HasModifiersFor env YogSothoth where
  getModifiersFor _ target (YogSothoth attrs) = case effectMetadata attrs of
    Just (EffectInt n) -> case target of
      EnemyTarget eid -> case effectSource attrs of
        EnemySource eid' | eid' == eid ->
          pure $ toModifiers attrs [HorrorDealt (-n)]
        _ -> pure []
      _ -> pure []
    _ -> pure []

instance HasQueue env => RunMessage env YogSothoth where
  runMessage msg e@(YogSothoth attrs) = case msg of
    DeckHasNoCards iid _ | isTarget attrs (InvestigatorTarget iid) ->
      e <$ push (DrivenInsane iid)
    _ -> YogSothoth <$> runMessage msg attrs
