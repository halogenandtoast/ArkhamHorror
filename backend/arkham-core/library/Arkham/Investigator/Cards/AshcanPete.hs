module Arkham.Investigator.Cards.AshcanPete
  ( AshcanPete(..)
  , ashcanPete
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding ( FastPlayerWindow )
import Arkham.Message
import Arkham.Modifier

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashcanPete :: InvestigatorCard AshcanPete
ashcanPete = investigatorWith
  AshcanPete
  Cards.ashcanPete
  Stats
    { health = 6
    , sanity = 5
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  (startsWithL .~ [Assets.duke])

instance HasAbilities AshcanPete where
  getAbilities (AshcanPete x) =
    [ limitedAbility (PlayerLimit PerRound 1) $ restrictedAbility
        x
        1
        (Self <> AssetExists (AssetControlledBy You <> AssetExhausted) <> Negate
          (SelfHasModifier ControlledAssetsCannotReady)
        )
        (FastAbility $ HandDiscardCost 1 AnyCard)
    ]

instance HasChaosTokenValue AshcanPete where
  getChaosTokenValue iid ElderSign (AshcanPete attrs) | iid == toId attrs =
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AshcanPete where
  runMessage msg i@(AshcanPete attrs) = case msg of
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      mduke <- selectOne $ assetIs Assets.duke
      for_ mduke $ \duke -> push $ Ready (AssetTarget duke)
      pure i
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      targets <- selectListMap
        AssetTarget
        (AssetControlledBy You <> AssetExhausted)
      push $ chooseOne
        iid
        [ TargetLabel target [Ready target] | target <- targets ]
      pure i
    _ -> AshcanPete <$> runMessage msg attrs
