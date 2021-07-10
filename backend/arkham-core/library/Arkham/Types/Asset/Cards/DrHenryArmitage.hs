module Arkham.Types.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.WindowMatcher

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

fastAbility :: AssetAttrs -> Ability
fastAbility a =
  (assetAbility
      a
      1
      (FastAbility $ Costs [DiscardJustDrawnCard, ExhaustCost (toTarget a)])
    )
    { abilityResponseWindow = Just (AfterDrawCard You AnyCard)
    }

instance HasModifiersFor env DrHenryArmitage

instance HasAbilities DrHenryArmitage where
  getAbilities (DrHenryArmitage a) = [fastAbility a]

instance (AssetRunner env) => RunMessage env DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 3 False)
    _ -> DrHenryArmitage <$> runMessage msg attrs
