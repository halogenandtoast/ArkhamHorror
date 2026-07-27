module Arkham.Asset.Assets.AncientRelicLeeringVisage (ancientRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

newtype AncientRelicLeeringVisage = AncientRelicLeeringVisage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientRelic :: AssetCard AncientRelicLeeringVisage
ancientRelic = assetWith AncientRelicLeeringVisage Cards.ancientRelic (healthL ?~ 5)

instance HasModifiersFor AncientRelicLeeringVisage where
  getModifiersFor (AncientRelicLeeringVisage a) =
    -- "If Ancient Relic would leave play, set it aside, out of play."
    modifySelf a [RemoveFromGameInsteadOfDiscard]

instance HasAbilities AncientRelicLeeringVisage where
  getAbilities (AncientRelicLeeringVisage a) =
    [ controlled_ a 1 $ forced $ DealtDamage #after AnySource You
    , controlled_ a 2 $ freeReaction $ GameEnds #when
    ]

instance RunMessage AncientRelicLeeringVisage where
  runMessage msg a@(AncientRelicLeeringVisage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- "Deal 1 damage to Ancient Relic (place 1 damage on this asset)."
      dealAssetDamage attrs.id (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      -- "Flip this card and resolve its text." The back (11581b) is a story card,
      -- which owns the glyph and the victory display.
      readStory iid attrs Stories.ancientRelic
      pure a
    _ -> AncientRelicLeeringVisage <$> liftRunMessage msg attrs
