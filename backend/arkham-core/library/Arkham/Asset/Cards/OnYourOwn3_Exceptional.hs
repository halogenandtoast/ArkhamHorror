module Arkham.Asset.Cards.OnYourOwn3_Exceptional (
  onYourOwn3_Exceptional,
  OnYourOwn3_Exceptional (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (PlayCard)
import Arkham.Card
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype OnYourOwn3_Exceptional = OnYourOwn3_Exceptional AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

onYourOwn3_Exceptional :: AssetCard OnYourOwn3_Exceptional
onYourOwn3_Exceptional = asset OnYourOwn3_Exceptional Cards.onYourOwn3_Exceptional

instance HasAbilities OnYourOwn3_Exceptional where
  getAbilities (OnYourOwn3_Exceptional a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (PlayCard #when You $ BasicCardMatch $ #survivor <> #event) (exhaust a)
    ]

instance HasModifiersFor OnYourOwn3_Exceptional where
  getModifiersFor (InvestigatorTarget iid) (OnYourOwn3_Exceptional attrs) =
    pure $ toModifiers attrs [CanReduceCostOf (#event <> #survivor) 2 | controlledBy attrs iid]
  getModifiersFor _ _ = pure []

getCardId :: [Window] -> CardId
getCardId [] = error "missing play card window"
getCardId ((windowType -> Window.PlayCard _ c) : _) = toCardId c
getCardId (_ : xs) = getCardId xs

instance RunMessage OnYourOwn3_Exceptional where
  runMessage msg a@(OnYourOwn3_Exceptional attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCardId -> cardId) _ -> do
      push $ costModifier (toAbilitySource attrs 1) iid (ReduceCostOf (CardWithId cardId) 2)
      pure a
    _ -> OnYourOwn3_Exceptional <$> runMessage msg attrs
