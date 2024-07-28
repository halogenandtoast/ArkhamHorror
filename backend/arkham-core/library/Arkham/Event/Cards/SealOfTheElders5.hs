module Arkham.Event.Cards.SealOfTheElders5 (
  sealOfTheElders5,
  SealOfTheElders5 (..),
)
where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Helpers.Window
import Arkham.Strategy

newtype SealOfTheElders5 = SealOfTheElders5 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealOfTheElders5 :: EventCard SealOfTheElders5
sealOfTheElders5 = eventWith SealOfTheElders5 Cards.sealOfTheElders5 $ afterPlayL .~ RemoveThisFromGame

instance RunMessage SealOfTheElders5 where
  runMessage msg e@(SealOfTheElders5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let tokens = getRevealedChaosTokens attrs.windows
      let curseCount = count ((== #curse) . (.face)) tokens
      let blessCount = count ((== #bless) . (.face)) tokens

      when (curseCount >= 2) do
        servantOfBrass <-
          fromJustNote "must be" . listToMaybe <$> searchBonded iid Assets.servantOfBrassDaemonaicVassal
        putCardIntoPlay iid servantOfBrass

      when (blessCount >= 2) do
        keeperOfTheKey <-
          fromJustNote "must be" . listToMaybe <$> searchBonded iid Assets.keeperOfTheKeyCelestialWard
        putCardIntoPlay iid keeperOfTheKey
      pure e
    _ -> SealOfTheElders5 <$> liftRunMessage msg attrs
