module Arkham.Event.Cards.MiracleWish5 (miracleWish5, MiracleWish5 (..)) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (searchBondedJust)
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens)
import Arkham.Token

newtype MiracleWish5 = MiracleWish5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miracleWish5 :: EventCard MiracleWish5
miracleWish5 = event MiracleWish5 Cards.miracleWish5

instance RunMessage MiracleWish5 where
  runMessage msg e@(MiracleWish5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      tokens <- count ((== #bless) . (.face)) <$> getSkillTestRevealedChaosTokens
      evanescentAscension <- searchBondedJust iid Assets.evanescentAscensionTheMorningStar
      putCardIntoPlay iid evanescentAscension
      placeTokens attrs evanescentAscension Wish tokens
      removeFromGame attrs
      pure e
    _ -> MiracleWish5 <$> liftRunMessage msg attrs
