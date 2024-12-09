module Arkham.Asset.Assets.ChuckFergus2 (chuckFergus2, ChuckFergus2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Cost
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Tactic, Trick))

newtype ChuckFergus2 = ChuckFergus2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chuckFergus2 :: AssetCard ChuckFergus2
chuckFergus2 = ally ChuckFergus2 Cards.chuckFergus2 (2, 2)

cardMatcher :: CardMatcher
cardMatcher = CardWithOneOf [CardWithTrait Tactic, CardWithTrait Trick] <> CardWithType EventType

-- If another card uses this we will need to figure out how to keep track of
-- which thing has applied which modifiers. I think we are safe with the
-- current card pool
instance HasModifiersFor ChuckFergus2 where
  getModifiersFor (ChuckFergus2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modifiedWhen_ a a.ready iid [ChuckFergus2Modifier cardMatcher 2]

instance HasAbilities ChuckFergus2 where
  getAbilities (ChuckFergus2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard #when You $ basic cardMatcher) (exhaust a)
    ]

instance RunMessage ChuckFergus2 where
  runMessage msg a@(ChuckFergus2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(cardPlayed -> card@(PlayerCard pc)) _ -> do
      cost <- getModifiedCardCost iid card
      canAffordCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ResourceCost cost)
      canAffordActionCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ActionCost 1)

      unless canAffordActionCost $ eventModifier attrs card $ BecomesFast FastPlayerWindow
      unless canAffordCost $ eventModifier attrs iid $ ReduceCostOf (CardWithId card.id) 2

      -- can something else reduce the cost enough?
      when (canAffordCost && canAffordActionCost) $ do
        chooseOneM iid do
          labeled "That event gains fast" $ eventModifier attrs card $ BecomesFast FastPlayerWindow
          labeled "That event costs 2 fewer resources to play." do
            eventModifier attrs iid $ ReduceCostOf (CardWithId card.id) 2
          labeled "You get +2 skill value while performing a skill test during the resolution of that event." do
            eventModifier attrs iid $ AnySkillValue 2

      pure a
    _ -> ChuckFergus2 <$> liftRunMessage msg attrs
