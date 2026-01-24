module Arkham.Asset.Assets.DrRosaMarquezBestInHerField (drRosaMarquezBestInHerField) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex, pattern Theta)
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype DrRosaMarquezBestInHerField = DrRosaMarquezBestInHerField AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drRosaMarquezBestInHerField :: AssetCard DrRosaMarquezBestInHerField
drRosaMarquezBestInHerField = asset DrRosaMarquezBestInHerField Cards.drRosaMarquezBestInHerField

instance HasModifiersFor DrRosaMarquezBestInHerField where
  getModifiersFor (DrRosaMarquezBestInHerField a) =
    controllerGets a [SkillModifier #intellect 1, SkillModifier #agility 1]

instance HasAbilities DrRosaMarquezBestInHerField where
  getAbilities (DrRosaMarquezBestInHerField a) =
    [ groupLimit PerGame
        $ controlled_ a 1
        $ freeReaction
        $ DiscoveringLastClue #after You YourLocation
    ]

instance RunMessage DrRosaMarquezBestInHerField where
  runMessage msg a@(DrRosaMarquezBestInHerField attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid Theta
      pure a
    _ -> DrRosaMarquezBestInHerField <$> liftRunMessage msg attrs
