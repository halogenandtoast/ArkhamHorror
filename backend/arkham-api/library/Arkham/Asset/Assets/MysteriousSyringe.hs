module Arkham.Asset.Assets.MysteriousSyringe (mysteriousSyringe) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose hiding (labeled)
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype MysteriousSyringe = MysteriousSyringe AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriousSyringe :: AssetCard MysteriousSyringe
mysteriousSyringe = asset MysteriousSyringe Cards.mysteriousSyringe

instance HasAbilities MysteriousSyringe where
  getAbilities (MysteriousSyringe a) =
    [ restricted a 1 (ControlsThis <> exists (NotInvestigator You))
        $ forced
        $ Matcher.InvestigatorDefeated #when ByAny You
    , restricted a 2 ControlsThis actionAbility
    ]

instance RunMessage MysteriousSyringe where
  runMessage msg a@(MysteriousSyringe attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ NotInvestigator $ InvestigatorWithId iid
      chooseOrRunOneM iid do
        targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ colocatedWith iid
      chooseOrRunOneM iid do
        targets investigators \iid' -> do
          name <- field InvestigatorName iid'
          remember $ BeenInjected $ labeled name iid'
          removeFromGame attrs
      pure a
    _ -> MysteriousSyringe <$> liftRunMessage msg attrs
