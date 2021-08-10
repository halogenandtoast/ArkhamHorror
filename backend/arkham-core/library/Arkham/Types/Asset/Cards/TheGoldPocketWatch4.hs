module Arkham.Types.Asset.Cards.TheGoldPocketWatch4
  ( theGoldPocketWatch4
  , TheGoldPocketWatch4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message hiding (When)
import Arkham.Types.Phase
import Arkham.Types.Restriction
import qualified Arkham.Types.Window as Window

newtype TheGoldPocketWatch4 = TheGoldPocketWatch4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGoldPocketWatch4 :: AssetCard TheGoldPocketWatch4
theGoldPocketWatch4 = asset TheGoldPocketWatch4 Cards.theGoldPocketWatch4

instance HasActions TheGoldPocketWatch4 where
  getActions (TheGoldPocketWatch4 attrs) =
    [ restrictedAbility
      attrs
      1
      OwnsThis
      (ReactionAbility (PhaseBegins When AnyPhase) Free)
    , restrictedAbility
      attrs
      2
      OwnsThis
      (ReactionAbility (PhaseEnds When AnyPhase) Free)
    ]

instance HasModifiersFor env TheGoldPocketWatch4

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheGoldPocketWatch4 where
  runMessage msg a@(TheGoldPocketWatch4 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ pushAll [RemoveFromGame (toTarget attrs), EndPhase]
    UseCardAbility _ source [Window.Window When (Window.PhaseEnds p)] 2 _
      | isSource attrs source -> do
        let
          phaseMsg = case p of
            MythosPhase -> BeginMythos
            InvestigationPhase -> BeginInvestigation
            EnemyPhase -> BeginEnemy
            UpkeepPhase -> BeginUpkeep
            ResolutionPhase -> error "should not be called in this situation"
            CampaignPhase -> error "should not be called in this situation"
        clearQueue
        a <$ pushAll [RemoveFromGame (toTarget attrs), phaseMsg]
    _ -> TheGoldPocketWatch4 <$> runMessage msg attrs
