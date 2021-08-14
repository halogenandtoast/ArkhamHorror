module Arkham.Types.Asset.Cards.DaisysToteBagAdvanced
  ( daisysToteBagAdvanced
  , DaisysToteBagAdvanced(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetCard DaisysToteBagAdvanced
daisysToteBagAdvanced = asset DaisysToteBagAdvanced Cards.daisysToteBagAdvanced

instance HasSet Trait env (InvestigatorId, CardId) => HasAbilities env DaisysToteBagAdvanced where
  getAbilities iid (WhenPlayCard who cardId) (DaisysToteBagAdvanced a)
    | ownedBy a iid && iid == who = do
      isTome <- elem Tome <$> getSet @Trait (iid, cardId)
      let
        ability =
          (mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))
            )
            { abilityMetadata = Just (TargetMetadata $ CardIdTarget cardId)
            }
      pure [ ability | isTome ]
  getAbilities iid window (DaisysToteBagAdvanced attrs) =
    getAbilities iid window attrs

instance HasModifiersFor env DaisysToteBagAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (DaisysToteBagAdvanced a)
    | ownedBy a iid = pure
      [toModifier a $ CanBecomeFast (Just AssetType, [Tome])]
  getModifiersFor _ _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Tome Nothing

instance AssetRunner env => RunMessage env DaisysToteBagAdvanced where
  runMessage msg a@(DaisysToteBagAdvanced attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      pushAll $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> runMessage msg attrs
    UseCardAbility _ source (Just (TargetMetadata (CardIdTarget cardId))) 1 _
      | isSource attrs source -> a
      <$ push (CreateEffect "90002" Nothing source (CardIdTarget cardId))
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs
