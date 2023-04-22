module Arkham.Asset.Cards.DrHenryArmitage
  ( DrHenryArmitage(..)
  , drHenryArmitage
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

instance HasAbilities DrHenryArmitage where
  getAbilities (DrHenryArmitage a) =
    [ restrictedAbility a 1 ControlsThis
      $ ReactionAbility
          (DrawCard Timing.After You (BasicCardMatch AnyCard) (DeckOf You))
      $ Costs [DiscardDrawnCardCost, ExhaustCost (toTarget a)]
    ]

instance RunMessage DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 3 (toAbilitySource attrs 1) False
      pure a
    _ -> DrHenryArmitage <$> runMessage msg attrs
