module Arkham.Investigator.Cards.AshcanPete (AshcanPete (..), ashcanPete) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Capability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

ashcanPete :: InvestigatorCard AshcanPete
ashcanPete =
  startsWith [Assets.duke]
    $ investigator AshcanPete Cards.ashcanPete
    $ Stats {health = 6, sanity = 5, willpower = 4, intellect = 2, combat = 2, agility = 3}

instance HasAbilities AshcanPete where
  getAbilities (AshcanPete x) =
    [ playerLimit PerRound
        $ selfAbility x 1 (exists (AssetControlledBy You <> #exhausted) <> can.have.assets.ready You)
        $ FastAbility (HandDiscardCost 1 #any)
    ]

instance HasChaosTokenValue AshcanPete where
  getChaosTokenValue iid ElderSign (AshcanPete attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AshcanPete where
  runMessage msg i@(AshcanPete attrs) = runQueueT $ case msg of
    ElderSignEffect (is attrs -> True) -> do
      whenM (can.have.assets.ready attrs.id) $ selectEach (assetIs Assets.duke) ready
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid (assetControlledBy iid <> #exhausted) ready
      pure i
    _ -> AshcanPete <$> liftRunMessage msg attrs
