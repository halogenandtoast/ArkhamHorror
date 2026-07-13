module Arkham.Treachery.Cards.OminousMoonlightCircusExMortis (ominousMoonlightCircusExMortis) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OminousMoonlightCircusExMortis = OminousMoonlightCircusExMortis TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousMoonlightCircusExMortis :: TreacheryCard OminousMoonlightCircusExMortis
ominousMoonlightCircusExMortis = treachery OminousMoonlightCircusExMortis Cards.ominousMoonlightCircusExMortis

instance RunMessage OminousMoonlightCircusExMortis where
  runMessage msg t@(OminousMoonlightCircusExMortis attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (chaosToken_ #moon) >>= traverse_ (sealChaosToken iid iid)
      gainSurge attrs
      pure t
    _ -> OminousMoonlightCircusExMortis <$> liftRunMessage msg attrs
