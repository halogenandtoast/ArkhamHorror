module Arkham.Investigator.Cards.RolandBanksParallel (rolandBanksParallel) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted hiding (choose)
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message.Lifted.Choose
import Arkham.Name hiding (labeled)

newtype RolandBanksParallel = RolandBanksParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rolandBanksParallel :: InvestigatorCard RolandBanksParallel
rolandBanksParallel =
  investigator RolandBanksParallel Cards.rolandBanksParallel
    $ Stats {health = 9, sanity = 5, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities RolandBanksParallel where
  getAbilities (RolandBanksParallel a) =
    [playerLimit PerGame $ restricted a 1 Self $ FastAbility Free]

instance HasChaosTokenValue RolandBanksParallel where
  getChaosTokenValue iid ElderSign (RolandBanksParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

directives :: InvestigatorId -> AssetMatcher
directives iid = assetControlledBy iid <> AssetWithTitle "Directive"

instance RunMessage RolandBanksParallel where
  runMessage msg i@(RolandBanksParallel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (directives iid) (flipOverBy iid (attrs.ability 1))
      pure i
    ElderSignEffect (is attrs -> True) -> do
      activeDirectives <-
        selectWithField AssetName
          $ directives attrs.id
          <> not_ (assetWithMetaKeyValue "ignore_regulation" True)

      chooseOneM attrs.id do
        for_ activeDirectives \(directive, name) ->
          labeled (fromMaybe "Unknown" $ nameSubtitle name) do
            setGlobal directive "ignore_regulation" True

      pure i
    _ -> RolandBanksParallel <$> liftRunMessage msg attrs
