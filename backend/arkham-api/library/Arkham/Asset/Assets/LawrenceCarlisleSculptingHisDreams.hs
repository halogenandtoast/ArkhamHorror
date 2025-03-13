module Arkham.Asset.Assets.LawrenceCarlisleSculptingHisDreams (lawrenceCarlisleSculptingHisDreams) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype LawrenceCarlisleSculptingHisDreams = LawrenceCarlisleSculptingHisDreams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawrenceCarlisleSculptingHisDreams :: AssetCard LawrenceCarlisleSculptingHisDreams
lawrenceCarlisleSculptingHisDreams = ally LawrenceCarlisleSculptingHisDreams Cards.lawrenceCarlisleSculptingHisDreams (1, 1)

instance HasModifiersFor LawrenceCarlisleSculptingHisDreams where
  getModifiersFor (LawrenceCarlisleSculptingHisDreams a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities LawrenceCarlisleSculptingHisDreams where
  getAbilities (LawrenceCarlisleSculptingHisDreams x) =
    [ controlled x 1 (can.draw.cards You)
        $ triggered (DiscardedFromHand #after You AnySource #any) (exhaust x)
    ]

instance RunMessage LawrenceCarlisleSculptingHisDreams where
  runMessage msg a@(LawrenceCarlisleSculptingHisDreams attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> LawrenceCarlisleSculptingHisDreams <$> liftRunMessage msg attrs
