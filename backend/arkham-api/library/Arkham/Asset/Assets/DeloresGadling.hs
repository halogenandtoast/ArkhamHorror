module Arkham.Asset.Assets.DeloresGadling (deloresGadling) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets, maybeModified_)
import Arkham.Helpers.SkillTest (isInvestigation, isParley)
import Arkham.Matcher
import Arkham.Trait

newtype DeloresGadling = DeloresGadling AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deloresGadling :: AssetCard DeloresGadling
deloresGadling = allyWith DeloresGadling Cards.deloresGadling (1, 3) noSlots

instance HasModifiersFor DeloresGadling where
  getModifiersFor (DeloresGadling a) = do
    controllerGets a [MayIgnoreAttacksOfOpportunity]
    for_ a.controller \iid -> maybeModified_ a iid do
      liftGuardM $ (||) <$> isInvestigation <*> isParley
      n <- selectCount $ EnemyAt YourLocation <> EnemyWithTrait Humanoid
      guard (n > 0)
      pure [SkillModifier #intellect n]

instance RunMessage DeloresGadling where
  runMessage msg (DeloresGadling attrs) = DeloresGadling <$> runMessage msg attrs
