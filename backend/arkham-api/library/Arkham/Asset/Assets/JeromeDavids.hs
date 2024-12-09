module Arkham.Asset.Assets.JeromeDavids (jeromeDavids, JeromeDavids (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype JeromeDavids = JeromeDavids AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromeDavids :: AssetCard JeromeDavids
jeromeDavids =
  allyWith JeromeDavids Cards.jeromeDavids (1, 4) (isStoryL .~ True)

instance HasModifiersFor JeromeDavids where
  getModifiersFor (JeromeDavids a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (DrawCard #when You (CanCancelRevelationEffect $ basic #treachery) EncounterDeck)
          (exhaust a <> SkillIconCost 2 (singleton #intellect))
    ]

instance RunMessage JeromeDavids where
  runMessage msg a@(JeromeDavids attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ CancelNext (toSource attrs) RevelationMessage
      pure a
    _ -> JeromeDavids <$> runMessage msg attrs
