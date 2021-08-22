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
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (PlayCard)
import Arkham.Types.Modifier
import Arkham.Types.Slot
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import qualified Arkham.Types.Window as Window

newtype DaisysToteBagAdvanced = DaisysToteBagAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisysToteBagAdvanced :: AssetCard DaisysToteBagAdvanced
daisysToteBagAdvanced = asset DaisysToteBagAdvanced Cards.daisysToteBagAdvanced

instance HasAbilities env DaisysToteBagAdvanced where
  getAbilities _ _ (DaisysToteBagAdvanced a) = pure
    [ restrictedAbility a 1 OwnsThis
      $ ReactionAbility
          (PlayCard Timing.When You (BasicCardMatch $ CardWithTrait Tome))
      $ ExhaustCost (toTarget a)
    ]

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
    UseCardAbility _ source [Window Timing.When (Window.PlayCard _ card)] 1 _
      | isSource attrs source
      -> a <$ push
        (CreateEffect "90002" Nothing source (CardIdTarget $ toCardId card))
    _ -> DaisysToteBagAdvanced <$> runMessage msg attrs
