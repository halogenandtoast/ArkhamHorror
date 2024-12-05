module Arkham.Asset.Assets.DrMilanChristopher where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Taboo.Types

newtype DrMilanChristopher = DrMilanChristopher AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drMilanChristopher :: AssetCard DrMilanChristopher
drMilanChristopher = ally DrMilanChristopher Cards.drMilanChristopher (1, 2)

instance HasModifiersFor DrMilanChristopher where
  getModifiersFor (DrMilanChristopher a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities DrMilanChristopher where
  getAbilities (DrMilanChristopher x) =
    [ playerLimit PerTestOrAbility
        $ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (SuccessfulInvestigation #after You Anywhere)
          (mwhen (maybe False (> TabooList15) x.taboo) (exhaust x))
    ]

instance RunMessage DrMilanChristopher where
  runMessage msg a@(DrMilanChristopher attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResourcesIfCan iid (attrs.ability 1) 1
      pure a
    _ -> DrMilanChristopher <$> liftRunMessage msg attrs
