module Arkham.Types.Asset.Cards.DaisysToteBagAdvanced
  ( daisysToteBagAdvanced
  , DaisysToteBagAdvanced(..)
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetId -> DaisysToteBagAdvanced
daisysToteBagAdvanced uuid = DaisysToteBagAdvanced $ baseAttrs uuid "90002"

instance HasSet Trait env (InvestigatorId, CardId) => HasActions env DaisysToteBagAdvanced where
  getActions iid (WhenPlayCard You cardId) (DaisysToteBagAdvanced a)
    | ownedBy a iid = do
      isTome <- elem Tome <$> getSet @Trait (iid, cardId)
      let
        ability =
          (mkAbility (toSource a) 1 (ReactionAbility $ ExhaustCost (toTarget a))
            )
            { abilityMetadata = Just (TargetMetadata $ CardIdTarget cardId)
            }
      pure [ ActivateCardAbilityAction iid ability | isTome ]
  getActions iid window (DaisysToteBagAdvanced attrs) =
    getActions iid window attrs

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
      unshiftMessages $ replicate 2 (AddSlot iid HandSlot (slot attrs))
      DaisysToteBagAdvanced <$> runMessage msg attrs
    UseCardAbility iid source (Just (TargetMetadata (CardIdTarget cardId))) 1 _
      | isSource attrs source
      -> a <$ unshiftMessage (ChangeCardToFast iid cardId)
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs
