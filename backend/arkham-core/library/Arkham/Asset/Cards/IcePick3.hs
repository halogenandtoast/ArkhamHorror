module Arkham.Asset.Cards.IcePick3 (icePick3, IcePick3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Matcher
import Arkham.Modifier

newtype IcePick3 = IcePick3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

icePick3 :: AssetCard IcePick3
icePick3 = asset IcePick3 Cards.icePick3

instance HasAbilities IcePick3 where
  getAbilities (IcePick3 x) =
    [ controlledAbility x 1 (DuringSkillTest $ oneOf [#investigating, #fighting])
        $ FastAbility (exhaust x)
    ]

instance RunMessage IcePick3 where
  runMessage msg a@(IcePick3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) iid (AnySkillValue 1)
      push $ AddSubscriber (toTarget attrs)
      pure a
    PassedSkillTest iid (Just action) _ (isTarget attrs -> True) _ _ | Just iid == attrs.controller -> do
      when (action == #fight) do
        chooseOne
          iid
          [ Label
              "Discard Ice Pick (3) to do +1 damage"
              [ Msg.toDiscardBy iid (attrs.ability 1) attrs
              , Msg.skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
              ]
          , Label "Do not Discard" []
          ]
      when (action == #investigate) do
        chooseOne
          iid
          [ Label
              "Discard Ice Pick (3) to discover 1 additional clue"
              [ Msg.toDiscardBy iid (attrs.ability 1) attrs
              , Msg.skillTestModifier (attrs.ability 1) iid (DiscoveredClues 1)
              ]
          , Label "Do not Discard" []
          ]
      pure a
    _ -> IcePick3 <$> liftRunMessage msg attrs
