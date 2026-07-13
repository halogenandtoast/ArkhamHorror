module Arkham.Homebrew.CircusExMortis.Treacheries.OminousMoonlight (ominousMoonlight) where

import Arkham.Matcher
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OminousMoonlight = OminousMoonlight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousMoonlight :: TreacheryCard OminousMoonlight
ominousMoonlight = treachery OminousMoonlight Cards.ominousMoonlight

instance RunMessage OminousMoonlight where
  runMessage msg t@(OminousMoonlight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (chaosToken_ #moon) >>= traverse_ (sealChaosToken iid iid)
      gainSurge attrs
      pure t
    _ -> OminousMoonlight <$> liftRunMessage msg attrs
