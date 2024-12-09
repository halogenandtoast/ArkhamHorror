module Arkham.Asset.Assets.Moonstone (moonstone, Moonstone (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (Discarded)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype Moonstone = Moonstone AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonstone :: AssetCard Moonstone
moonstone = asset Moonstone Cards.moonstone

instance HasModifiersFor Moonstone where
  getModifiersFor (Moonstone a) = controllerGets a [SkillModifier #willpower 1, SkillModifier #agility 1]

instance HasAbilities Moonstone where
  getAbilities (Moonstone x) =
    [ restrictedAbility x 1 InYourDiscard
        $ freeReaction
        $ Discarded #after (Just You) AnySource
        $ PlayableCardWithCriteria NoAction (CriteriaOverride NoRestriction)
        $ basic
        $ CardWithId x.cardId
    ]

instance RunMessage Moonstone where
  runMessage msg a@(Moonstone attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      payCardCost iid attrs
      putCardIntoPlay iid attrs
      pure a
    _ -> Moonstone <$> liftRunMessage msg attrs
