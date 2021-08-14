module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

fastAbility :: AssetAttrs -> Card -> Ability
fastAbility a card = mkAbility
  (toSource a)
  1
  (FastAbility $ Costs [DiscardCardCost card, ExhaustCost (toTarget a)])

instance HasModifiersFor env DrHenryArmitage

instance HasAbilities env DrHenryArmitage where
  getAbilities iid (AfterDrawCard who card) (DrHenryArmitage a)
    | ownedBy a iid && iid == who = pure [fastAbility a card]
  getAbilities _ _ _ = pure []

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 3 False)
    _ -> DrHenryArmitage <$> runMessage msg attrs
