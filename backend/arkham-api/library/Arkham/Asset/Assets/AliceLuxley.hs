module Arkham.Asset.Assets.AliceLuxley (aliceLuxley) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AliceLuxley = AliceLuxley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aliceLuxley :: AssetCard AliceLuxley
aliceLuxley = ally AliceLuxley Cards.aliceLuxley (2, 2)

instance HasModifiersFor AliceLuxley where
  getModifiersFor (AliceLuxley a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities AliceLuxley where
  getAbilities (AliceLuxley a) =
    [ controlled a 1 (canDamageEnemyAt (a.ability 1) YourLocation)
        $ triggered (DiscoverClues #after You Anywhere $ atLeast 1) (exhaust a)
    ]

instance RunMessage AliceLuxley where
  runMessage msg a@(AliceLuxley attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseDamageEnemy iid attrs (locationWithInvestigator iid) AnyEnemy 1
      pure a
    _ -> AliceLuxley <$> liftRunMessage msg attrs
