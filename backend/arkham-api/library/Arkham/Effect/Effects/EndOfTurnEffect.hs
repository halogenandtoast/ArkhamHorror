module Arkham.Effect.Effects.EndOfTurnEffect (
  EndOfTurnEffect (..),
  endOfTurnEffect,
  endOfTurnEffect',
) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Id
import Arkham.Prelude

newtype EndOfTurnEffect = EndOfTurnEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endOfTurnEffect :: EffectArgs -> EndOfTurnEffect
endOfTurnEffect = EndOfTurnEffect . uncurry (baseAttrs "eotef")

endOfTurnEffect'
  :: EffectId
  -> Source
  -> InvestigatorId
  -> [Message]
  -> EndOfTurnEffect
endOfTurnEffect' eid source iid msgs =
  EndOfTurnEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = InvestigatorTarget iid
      , effectCardCode = "eotef"
      , effectMetadata = Just (EffectMessages msgs)
      , effectTraits = mempty
      , effectWindow = Nothing
      , effectFinished = False
      , effectExtraMetadata = Null
      , effectSkillTest = Nothing
      , effectCardId = Nothing
      }

instance HasModifiersFor EndOfTurnEffect

instance RunMessage EndOfTurnEffect where
  runMessage msg e@(EndOfTurnEffect attrs) = runQueueT $ case msg of
    EndTurn iid | isTarget iid attrs.target -> do
      case attrs.metadata of
        Just (EffectMessages msgs) -> do
          pushAll $ DisableEffect attrs.id : msgs
        _ -> push $ DisableEffect attrs.id
      pure e
    _ -> EndOfTurnEffect <$> liftRunMessage msg attrs
