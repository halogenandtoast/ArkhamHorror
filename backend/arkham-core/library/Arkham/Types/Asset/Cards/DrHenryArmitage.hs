module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

fastAbility :: AssetAttrs -> CardId -> Ability
fastAbility a cid = mkAbility
  (toSource a)
  1
  (FastAbility $ Costs [DiscardCardCost cid, ExhaustCost (toTarget a)])

instance HasModifiersFor env DrHenryArmitage where
  getModifiersFor = noModifiersFor

instance HasActions env DrHenryArmitage where
  getActions iid (AfterDrawCard You cid) (DrHenryArmitage a) | ownedBy a iid =
    pure [ActivateCardAbilityAction iid (fastAbility a cid)]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 3 False)
    _ -> DrHenryArmitage <$> runMessage msg attrs
