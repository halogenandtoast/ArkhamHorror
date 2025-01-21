module Arkham.Asset.Assets.JeromeDavids (jeromeDavids) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (controllerGets, ModifierType (..))
import Arkham.Helpers.Window (drawnCard, DrawnCard(..))
import Arkham.Matcher

newtype JeromeDavids = JeromeDavids AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jeromeDavids :: AssetCard JeromeDavids
jeromeDavids = ally JeromeDavids Cards.jeromeDavids (1, 4)

instance HasModifiersFor JeromeDavids where
  getModifiersFor (JeromeDavids a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities JeromeDavids where
  getAbilities (JeromeDavids a) =
    [ restricted a 1 ControlsThis
        $ triggered
          (DrawCard #when You (CanCancelRevelationEffect $ basic #treachery) EncounterDeck)
          (exhaust a <> SkillIconCost 2 (singleton #intellect))
    ]

instance RunMessage JeromeDavids where
  runMessage msg a@(JeromeDavids attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (drawnCard -> drawn) _ -> do
      cancelRevelation (attrs.ability 1) drawn.card
      pure a
    _ -> JeromeDavids <$> liftRunMessage msg attrs
