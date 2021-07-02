module Arkham.Types.Asset.Cards.GrotesqueStatue4
  ( GrotesqueStatue4(..)
  , grotesqueStatue4
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.ChaosBagStepState
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window

newtype GrotesqueStatue4 = GrotesqueStatue4 AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

grotesqueStatue4 :: AssetCard GrotesqueStatue4
grotesqueStatue4 = hand GrotesqueStatue4 Cards.grotesqueStatue4

instance HasModifiersFor env GrotesqueStatue4 where
  getModifiersFor = noModifiersFor

ability :: AssetAttrs -> Source -> Ability
ability attrs source = base
  { abilityMetadata = Just (SourceMetadata source)
  , abilityLimit = PlayerLimit PerTestOrAbility 1 -- TODO: not a real limit
  }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ReactionAbility $ UseCost (toId attrs) Charge 1)

instance HasActions env GrotesqueStatue4 where
  getActions iid (WhenWouldRevealChaosToken source You) (GrotesqueStatue4 a)
    | ownedBy a iid = pure [ActivateCardAbilityAction iid (ability a source)]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env GrotesqueStatue4 where
  runMessage msg a@(GrotesqueStatue4 attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      GrotesqueStatue4 <$> runMessage msg (attrs & usesL .~ Uses Charge 4)
    UseCardAbility iid source (Just (SourceMetadata drawSource)) 1 _
      | isSource attrs source -> do
        when (useCount (assetUses attrs) == 1)
          $ unshiftMessage (Discard (toTarget attrs))
        a <$ unshiftMessage
          (ReplaceCurrentDraw drawSource iid
          $ Choose 1 [Undecided Draw, Undecided Draw] []
          )
    _ -> GrotesqueStatue4 <$> runMessage msg attrs
