module Arkham.Asset.Cards.OnYourOwn3
  ( onYourOwn3
  , OnYourOwn3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( PlayCard )
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Placement
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype OnYourOwn3 = OnYourOwn3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onYourOwn3 :: AssetCard OnYourOwn3
onYourOwn3 = asset OnYourOwn3 Cards.onYourOwn3

instance HasAbilities OnYourOwn3 where
  getAbilities (OnYourOwn3 a) = case assetPlacement a of
    InPlayArea iid ->
      [ restrictedAbility
          a
          0
          (AssetExists $ assetControlledBy iid <> AssetInSlot AllySlot)
        $ SilentForcedAbility AnyWindow
      , onYourOwn3Reaction
      ]
    _ -> [onYourOwn3Reaction]
   where
    onYourOwn3Reaction = restrictedAbility a 1 (ControlsThis) $ ReactionAbility
      (PlayCard Timing.When You (BasicCardMatch $ CardWithClass Survivor))
      (ExhaustCost (toTarget a))

instance HasModifiersFor OnYourOwn3 where
  getModifiersFor (InvestigatorTarget iid) (OnYourOwn3 attrs) =
    pure $ toModifiers
      attrs
      [ CanReduceCostOf (CardWithType EventType <> CardWithClass Survivor) 2
      | controlledBy attrs iid
      ]
  getModifiersFor _ _ = pure []

instance RunMessage OnYourOwn3 where
  runMessage msg a@(OnYourOwn3 attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 0 _ _ -> do
      push $ Discard (toTarget attrs)
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 _ _ -> do
      pure a
    _ -> OnYourOwn3 <$> runMessage msg attrs
