module Arkham.Asset.Assets.ErynnMacAoidhDevotedEnchantress (erynnMacAoidhDevotedEnchantress) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Matcher
import Arkham.Trait (Trait (Power))

newtype ErynnMacAoidhDevotedEnchantress = ErynnMacAoidhDevotedEnchantress AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erynnMacAoidhDevotedEnchantress :: AssetCard ErynnMacAoidhDevotedEnchantress
erynnMacAoidhDevotedEnchantress = ally ErynnMacAoidhDevotedEnchantress Cards.erynnMacAoidhDevotedEnchantress (2, 2)

instance HasModifiersFor ErynnMacAoidhDevotedEnchantress where
  getModifiersFor (ErynnMacAoidhDevotedEnchantress a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities ErynnMacAoidhDevotedEnchantress where
  getAbilities (ErynnMacAoidhDevotedEnchantress x) =
    [ controlled_ x 1
        $ triggered
          ( DrawCard
              #when
              You
              (CanCancelRevelationEffect You $ basic $ NonWeaknessTreachery <> oneOf [#curse, CardWithTrait Power])
              EncounterDeck
          )
          (assetUseCost x Secret 1 <> exhaust x)
    ]

instance RunMessage ErynnMacAoidhDevotedEnchantress where
  runMessage msg a@(ErynnMacAoidhDevotedEnchantress attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelRevelation attrs card
      pure a
    _ -> ErynnMacAoidhDevotedEnchantress <$> liftRunMessage msg attrs
