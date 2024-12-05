module Arkham.Asset.Assets.ChuckFergus5 (chuckFergus5, ChuckFergus5 (..)) where

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

newtype ChuckFergus5 = ChuckFergus5 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chuckFergus5 :: AssetCard ChuckFergus5
chuckFergus5 = ally ChuckFergus5 Cards.chuckFergus5 (2, 2)

cardMatcher :: CardMatcher
cardMatcher = CardWithOneOf [CardWithTrait Tactic, CardWithTrait Trick] <> CardWithType EventType

instance HasModifiersFor ChuckFergus5 where
  getModifiersFor (ChuckFergus5 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> modifiedWhen_ a a.ready iid [CanBecomeFast cardMatcher, CanReduceCostOf cardMatcher 2]

instance HasAbilities ChuckFergus5 where
  getAbilities (ChuckFergus5 a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard #when You $ basic cardMatcher) (exhaust a)
    ]

instance RunMessage ChuckFergus5 where
  runMessage msg a@(ChuckFergus5 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws@(cardPlayed -> card@(PlayerCard pc)) _ -> do
      cost <- getModifiedCardCost iid card
      canAffordCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ResourceCost cost)
      canAffordActionCost <- getCanAffordCost iid (CardIdSource pc.id) [#play] ws (ActionCost 1)
      -- can something else reduce the cost enough?
      let n = if canAffordCost then 2 else 1
          n' = if canAffordActionCost then n else n - 1
      unless canAffordActionCost $ eventModifier attrs card $ BecomesFast FastPlayerWindow
      unless canAffordCost $ eventModifier attrs iid $ ReduceCostOf (CardWithId card.id) 2
      when (n' > 0) $ do
        chooseNM iid n' do
          when canAffordActionCost do
            labeled "That event gains fast" $ eventModifier attrs card $ BecomesFast FastPlayerWindow

          when canAffordCost do
            labeled "That event costs 2 fewer resources to play." do
              eventModifier attrs iid $ ReduceCostOf (CardWithId card.id) 2

          labeled "You get +2 skill value while performing a skill test during the resolution of that event." do
            eventModifier attrs iid $ AnySkillValue 2

      pure a
    _ -> ChuckFergus5 <$> liftRunMessage msg attrs
