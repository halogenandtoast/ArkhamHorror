module Arkham.Types.Effect.Effects.PushedIntoTheBeyond
  ( PushedIntoTheBeyond(..)
  , pushedIntoTheBeyond
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Exception
import Arkham.Types.Message
import Arkham.Types.Target

newtype PushedIntoTheBeyond = PushedIntoTheBeyond EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pushedIntoTheBeyond :: EffectArgs -> PushedIntoTheBeyond
pushedIntoTheBeyond = PushedIntoTheBeyond . uncurry4 (baseAttrs "02100")

instance HasModifiersFor env PushedIntoTheBeyond

instance HasQueue env => RunMessage env PushedIntoTheBeyond where
  runMessage msg e@(PushedIntoTheBeyond attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId ->
      e <$ push (DiscardTopOfDeck iid 3 (Just $ EffectTarget eid))
    DiscardedTopOfDeck iid cards (EffectTarget eid) | eid == effectId ->
      case effectMetadata of
        Just (EffectCardCode x) -> e <$ when
          (x `elem` map (cdCardCode . toCardDef) cards)
          (push (InvestigatorAssignDamage iid effectSource DamageAny 0 2))
        _ -> throwIO (InvalidState "Must have one card as the target")
    _ -> PushedIntoTheBeyond <$> runMessage msg attrs
