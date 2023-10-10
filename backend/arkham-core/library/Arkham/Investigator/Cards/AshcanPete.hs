module Arkham.Investigator.Cards.AshcanPete (
  AshcanPete (..),
  ashcanPete,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Modifier

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashcanPete :: InvestigatorCard AshcanPete
ashcanPete =
  startsWith [Assets.duke]
    $ investigator AshcanPete Cards.ashcanPete
    $ Stats {health = 6, sanity = 5, willpower = 4, intellect = 2, combat = 2, agility = 3}

instance HasAbilities AshcanPete where
  getAbilities (AshcanPete x) =
    [ playerLimit PerRound
        $ withCriteria (mkAbility x 1 (FastAbility $ HandDiscardCost 1 AnyCard))
        $ Self
        <> AssetExists (AssetControlledBy You <> AssetExhausted)
        <> Negate (SelfHasModifier ControlledAssetsCannotReady)
    ]

instance HasChaosTokenValue AshcanPete where
  getChaosTokenValue iid ElderSign (AshcanPete attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AshcanPete where
  runMessage msg i@(AshcanPete attrs) = case msg of
    ResolveChaosToken _drawnToken ElderSign iid | attrs `is` iid -> do
      mduke <- selectOne $ assetIs Assets.duke
      for_ mduke $ push . ready
      pure i
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      targets <- selectList $ assetControlledBy iid <> AssetExhausted
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels targets (only . ready)
      pure i
    _ -> AshcanPete <$> runMessage msg attrs
