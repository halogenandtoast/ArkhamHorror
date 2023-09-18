module Arkham.Asset.Cards.OnYourOwn3 (
  onYourOwn3,
  OnYourOwn3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Matcher
import Arkham.Placement
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Metadata = Metadata {beingDiscarded :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype OnYourOwn3 = OnYourOwn3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onYourOwn3 :: AssetCard OnYourOwn3
onYourOwn3 = asset (OnYourOwn3 . (`with` Metadata False)) Cards.onYourOwn3

instance HasAbilities OnYourOwn3 where
  getAbilities (OnYourOwn3 (a `With` meta)) = case assetPlacement a of
    InPlayArea iid ->
      [ restrictedAbility
        a
        0
        (AssetExists $ assetControlledBy iid <> AssetInSlot AllySlot)
        $ SilentForcedAbility AnyWindow
      | not (beingDiscarded meta)
      ]
        <> [onYourOwn3Reaction]
    _ -> [onYourOwn3Reaction]
   where
    onYourOwn3Reaction =
      restrictedAbility a 1 ControlsThis
        $ ReactionAbility (PlayCard #when You $ BasicCardMatch $ #survivor <> #event) (exhaust a)

instance HasModifiersFor OnYourOwn3 where
  getModifiersFor (InvestigatorTarget iid) (OnYourOwn3 (attrs `With` _)) =
    pure $ toModifiers attrs [CanReduceCostOf (#event <> #survivor) 2 | controlledBy attrs iid]
  getModifiersFor _ _ = pure []

getCardId :: [Window] -> CardId
getCardId [] = error "missing play card window"
getCardId ((windowType -> Window.PlayCard _ c) : _) = toCardId c
getCardId (_ : xs) = getCardId xs

instance RunMessage OnYourOwn3 where
  runMessage msg a@(OnYourOwn3 (attrs `With` meta)) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 0 _ _ -> do
      push $ Discard GameSource (toTarget attrs)
      pure . OnYourOwn3 $ attrs `with` Metadata True
    UseCardAbility iid (isSource attrs -> True) 1 (getCardId -> cardId) _ -> do
      push $ costModifier (toAbilitySource attrs 1) iid (ReduceCostOf (CardWithId cardId) 2)
      pure a
    _ -> OnYourOwn3 . (`with` meta) <$> runMessage msg attrs
