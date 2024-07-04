module Arkham.Asset.Cards.RodOfAnimalism1 (rodOfAnimalism1, RodOfAnimalism1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Card
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Slot
import Arkham.Trait (Trait (Creature))

newtype RodOfAnimalism1 = RodOfAnimalism1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rodOfAnimalism1 :: AssetCard RodOfAnimalism1
rodOfAnimalism1 = asset RodOfAnimalism1 Cards.rodOfAnimalism1

instance HasModifiersFor RodOfAnimalism1 where
  getModifiersFor (InvestigatorTarget iid) (RodOfAnimalism1 attrs) =
    pure
      $ toModifiers attrs [CanReduceCostOf (#asset <> CardWithTrait Creature) 1 | controlledBy attrs iid]
  getModifiersFor _ _ = pure []

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Creature []

instance HasAbilities RodOfAnimalism1 where
  getAbilities (RodOfAnimalism1 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ freeReaction (PlayCard #when You $ basic $ #asset <> CardWithTrait Creature)
    ]

instance RunMessage RodOfAnimalism1 where
  runMessage msg a@(RodOfAnimalism1 attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      pushAll $ replicate 2 (AddSlot iid #ally (slot attrs))
      RodOfAnimalism1 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      let source = toAbilitySource attrs 1
      costModifier source iid (ReduceCostOf (CardWithId $ toCardId card) 1)
      pure a
    _ -> RodOfAnimalism1 <$> liftRunMessage msg attrs
