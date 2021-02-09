module Arkham.Types.Asset.Cards.ArcaneInitiate where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Trait

newtype ArcaneInitiate = ArcaneInitiate AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneInitiate :: AssetId -> ArcaneInitiate
arcaneInitiate uuid = ArcaneInitiate $ (baseAttrs uuid "01063")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

fastAbility :: AssetAttrs -> Ability
fastAbility a =
  mkAbility (toSource a) 1 (FastAbility $ ExhaustCost (toTarget a))

instance HasModifiersFor env ArcaneInitiate where
  getModifiersFor = noModifiersFor

instance HasActions env ArcaneInitiate where
  getActions iid FastPlayerWindow (ArcaneInitiate a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid $ fastAbility a]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ArcaneInitiate where
  runMessage msg a@(ArcaneInitiate attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ArcaneInitiate <$> runMessage msg (attrs & doomL +~ 1)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ unshiftMessage
      (chooseOne
        iid
        [SearchTopOfDeck iid (InvestigatorTarget iid) 3 [Spell] ShuffleBackIn]
      )
    _ -> ArcaneInitiate <$> runMessage msg attrs
