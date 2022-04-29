module Arkham.Investigator.Cards.AshcanPete
  ( AshcanPete(..)
  , ashcanPete
  ) where

import Arkham.Prelude

import Arkham.Ability
import qualified Arkham.Asset.Cards as Assets
import Arkham.Cost
import Arkham.Criteria
import qualified Arkham.Investigator.Cards as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (FastPlayerWindow)
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype AshcanPete = AshcanPete InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor env)
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
    [ restrictedAbility
          x
          1
          (Self <> AssetExists (AssetOwnedBy You <> AssetExhausted) <> Negate
            (SelfHasModifier ControlledAssetsCannotReady)
          )
          (FastAbility $ HandDiscardCost 1 AnyCard)
        & abilityLimitL
        .~ PlayerLimit PerRound 1
    ]

instance HasTokenValue env AshcanPete where
  getTokenValue (AshcanPete attrs) iid ElderSign | iid == toId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue _ _ token = pure $ TokenValue token mempty

instance InvestigatorRunner env => RunMessage env AshcanPete where
  runMessage msg i@(AshcanPete attrs) = case msg of
    ResolveToken _drawnToken ElderSign iid | iid == toId attrs ->
      i <$ push (Ready $ CardCodeTarget "02014")
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      targets <- selectListMap AssetTarget (AssetOwnedBy You <> AssetExhausted)
      i <$ push (chooseOne (toId attrs) [ Ready target | target <- targets ])
    _ -> AshcanPete <$> runMessage msg attrs
