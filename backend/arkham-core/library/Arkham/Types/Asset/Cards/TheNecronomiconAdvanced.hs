module Arkham.Types.Asset.Cards.TheNecronomiconAdvanced
  ( TheNecronomiconAdvanced(..)
  , theNecronomiconAdvanced
  ) where


import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import qualified Arkham.Types.Token as Token

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetId -> TheNecronomiconAdvanced
theNecronomiconAdvanced uuid =
  TheNecronomiconAdvanced $ (baseAttrs uuid "90003")
    { assetSlots = [HandSlot]
    , assetHorror = Just 3
    , assetCanLeavePlayByNormalMeans = False
    }

instance HasModifiersFor env TheNecronomiconAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconAdvanced a) =
    pure $ toModifiers
      a
      [ ForcedTokenChange
          Token.ElderSign
          [Token.Cultist, Token.Tablet, Token.ElderThing]
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions env TheNecronomiconAdvanced where
  getActions iid NonFast (TheNecronomiconAdvanced a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility Nothing $ ActionCost 1))
    | fromJustNote "Must be set" (assetHorror a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ unshiftMessage (PlayCard iid (getCardId attrs) Nothing False)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      unshiftMessage $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ unshiftMessage (Discard (toTarget attrs))
        else pure $ TheNecronomiconAdvanced
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
