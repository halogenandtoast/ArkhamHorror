module Arkham.Event.Cards.UncannyGrowth (
  uncannyGrowth,
  UncannyGrowth (..),
)
where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Token

newtype UncannyGrowth = UncannyGrowth EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncannyGrowth :: EventCard UncannyGrowth
uncannyGrowth = event UncannyGrowth Cards.uncannyGrowth

instance RunMessage UncannyGrowth where
  runMessage msg e@(UncannyGrowth attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid attrs
      pure e
    PassedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      afterSkillTest do
        mMyconid <- selectOne $ assetControlledBy iid <> AssetWithTitle "Ravenous Myconid"
        for_ mMyconid \myconid -> do
          placeTokens attrs myconid Growth n
        placeInBonded iid attrs
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      returnToHand iid attrs
      pure e
    _ -> UncannyGrowth <$> liftRunMessage msg attrs
