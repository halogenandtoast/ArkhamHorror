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
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Target
import Arkham.Types.Window

newtype TheGoldPocketWatch4 = TheGoldPocketWatch4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGoldPocketWatch4 :: AssetCard TheGoldPocketWatch4
theGoldPocketWatch4 = asset TheGoldPocketWatch4 Cards.theGoldPocketWatch4

skipPhaseAbility :: Phase -> AssetAttrs -> Ability
skipPhaseAbility _ attrs = mkAbility attrs 1 (ReactionAbility Free)

repeatPhaseAbility :: Phase -> AssetAttrs -> Ability
repeatPhaseAbility p attrs = (mkAbility attrs 2 (ReactionAbility Free))
  { abilityMetadata = Just (TargetMetadata $ PhaseTarget p)
  }

instance HasActions env TheGoldPocketWatch4 where
  getActions iid (PhaseBegins p) (TheGoldPocketWatch4 attrs)
    | ownedBy attrs iid = pure [skipPhaseAbility p attrs]
  getActions iid (PhaseEnds p) (TheGoldPocketWatch4 attrs) | ownedBy attrs iid =
    pure [repeatPhaseAbility p attrs]
  getActions _ _ _ = pure []

instance HasModifiersFor env TheGoldPocketWatch4

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheGoldPocketWatch4 where
  runMessage msg a@(TheGoldPocketWatch4 attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ pushAll [RemoveFromGame (toTarget attrs), EndPhase]
    UseCardAbility _ source (Just (TargetMetadata (PhaseTarget p))) 2 _
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
