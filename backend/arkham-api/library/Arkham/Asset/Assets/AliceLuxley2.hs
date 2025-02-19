module Arkham.Asset.Assets.AliceLuxley2 (aliceLuxley2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype AliceLuxley2 = AliceLuxley2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aliceLuxley2 :: AssetCard AliceLuxley2
aliceLuxley2 = ally AliceLuxley2 Cards.aliceLuxley2 (2, 2)

instance HasModifiersFor AliceLuxley2 where
  getModifiersFor (AliceLuxley2 a) = do
    controllerGets a [SkillModifier #intellect 1]
    for_ a.controller \iid -> do
      actions <- fieldMap InvestigatorActionsTaken concat iid
      modified_ a iid $ do
        guard $ #investigate `notElem` actions
        pure $ ActionDoesNotCauseAttacksOfOpportunity #investigate

instance HasAbilities AliceLuxley2 where
  getAbilities (AliceLuxley2 a) =
    [ controlled a 1 (exists (at_ YourLocation <> (a.ability 1).canDamage) <> CanDealDamage)
        $ triggered (DiscoverClues #after You Anywhere $ atLeast 1) (exhaust a)
    ]

instance RunMessage AliceLuxley2 where
  runMessage msg a@(AliceLuxley2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ at_ (locationWithInvestigator iid) <> (attrs.ability 1).canDamage
      chooseOrRunOneM iid $ targets enemies $ nonAttackEnemyDamage (attrs.ability 1) 1
      pure a
    _ -> AliceLuxley2 <$> liftRunMessage msg attrs
