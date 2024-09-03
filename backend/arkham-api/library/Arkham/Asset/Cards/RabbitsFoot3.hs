module Arkham.Asset.Cards.RabbitsFoot3 (RabbitsFoot3 (..), rabbitsFoot3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RabbitsFoot3 = RabbitsFoot3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rabbitsFoot3 :: AssetCard RabbitsFoot3
rabbitsFoot3 = asset RabbitsFoot3 Cards.rabbitsFoot3

instance HasAbilities RabbitsFoot3 where
  getAbilities (RabbitsFoot3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (SkillTestResult #after You #any #failure) (exhaust a)
    ]

instance RunMessage RabbitsFoot3 where
  runMessage msg a@(RabbitsFoot3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 [(windowType -> Window.FailSkillTest _ x)] _ -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck x] #any (DrawFound iid 1)
      pure a
    _ -> RabbitsFoot3 <$> liftRunMessage msg attrs
