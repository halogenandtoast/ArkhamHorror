module Arkham.Effect.Effects.PushedIntoTheBeyond (
  PushedIntoTheBeyond (..),
  pushedIntoTheBeyond,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Exception
import Arkham.Message

newtype PushedIntoTheBeyond = PushedIntoTheBeyond EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: EffectArgs -> PushedIntoTheBeyond
pushedIntoTheBeyond = PushedIntoTheBeyond . uncurry4 (baseAttrs "02100")

instance RunMessage PushedIntoTheBeyond where
  runMessage msg e@(PushedIntoTheBeyond attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid)
      | eid == effectId ->
          e <$ push (DiscardTopOfDeck iid 3 effectSource (Just $ EffectTarget eid))
    DiscardedTopOfDeck iid cards _ (EffectTarget eid) | eid == effectId ->
      case effectMetadata of
        Just (EffectCardCodes [x]) ->
          e
            <$ when
              (x `elem` map (cdCardCode . toCardDef) cards)
              (push (InvestigatorAssignDamage iid effectSource DamageAny 0 2))
        _ -> throwIO (InvalidState "Must have one card as the target")
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
