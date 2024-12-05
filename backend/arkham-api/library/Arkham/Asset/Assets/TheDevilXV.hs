module Arkham.Asset.Assets.TheDevilXV (theDevilXv, TheDevilXV (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype TheDevilXV = TheDevilXV AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilXv :: AssetCard TheDevilXV
theDevilXv = asset TheDevilXV Cards.theDevilXv

instance HasModifiersFor TheDevilXV where
  getModifiersFor (TheDevilXV a) = case a.placement of
    StillInHand iid -> modified_ a iid [CannotPlay $ #asset <> NotCard (CardWithId $ toCardId a)]
    _ -> pure mempty

instance RunMessage TheDevilXV where
  runMessage msg (TheDevilXV attrs) = TheDevilXV <$> runMessage msg attrs
