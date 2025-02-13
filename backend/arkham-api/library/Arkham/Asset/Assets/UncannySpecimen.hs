module Arkham.Asset.Assets.UncannySpecimen (uncannySpecimen) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher

newtype UncannySpecimen = UncannySpecimen AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncannySpecimen :: AssetCard UncannySpecimen
uncannySpecimen = asset UncannySpecimen Cards.uncannySpecimen

instance HasModifiersFor UncannySpecimen where
  getModifiersFor (UncannySpecimen a) = modifySelf a [SharesSlotWith 3 "Uncanny Specimen"]

instance HasAbilities UncannySpecimen where
  getAbilities (UncannySpecimen a) =
    [ controlled a 1 (DuringSkillTest SkillTestAtYourLocation)
        $ triggered
          ( RevealChaosToken #when Anyone
              $ oneOf [#skull, #cultist, #tablet, #elderthing]
          )
          (discardCost a)
    ]

instance RunMessage UncannySpecimen where
  runMessage msg a@(UncannySpecimen attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      withSkillTest \sid ->
        onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ doStep 1 msg
      cancelChaosToken (attrs.ability 1) iid token
      returnChaosTokens [token]
      unfocusChaosTokens
      drawAnotherChaosToken iid
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> UncannySpecimen <$> liftRunMessage msg attrs
