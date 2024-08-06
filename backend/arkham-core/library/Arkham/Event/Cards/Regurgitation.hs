module Arkham.Event.Cards.Regurgitation (
  regurgitation,
  Regurgitation (..),
)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Regurgitation = Regurgitation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

regurgitation :: EventCard Regurgitation
regurgitation = event Regurgitation Cards.regurgitation

instance RunMessage Regurgitation where
  runMessage msg e@(Regurgitation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      mRavenous <- selectOne $ assetIs Assets.ravenousUncontrolledHunger
      for_ mRavenous $ \ravenous -> do
        push $ Flip iid (toSource attrs) (toTarget ravenous)
      push $ ForTarget (toTarget iid) msg -- back over to 5U-21
      pure e
    _ -> Regurgitation <$> liftRunMessage msg attrs
