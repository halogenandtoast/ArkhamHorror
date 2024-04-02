module Arkham.Asset.Cards.Blackjack (blackjack, Blackjack (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype Blackjack = Blackjack AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackjack :: AssetCard Blackjack
blackjack = asset Blackjack Cards.blackjack

instance HasAbilities Blackjack where
  getAbilities (Blackjack a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage Blackjack where
  runMessage msg a@(Blackjack attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ skillTestModifiers source iid [SkillModifier #combat 1, DoesNotDamageOtherInvestigator]
        , chooseFight
        ]
      pure a
    _ -> Blackjack <$> runMessage msg attrs
