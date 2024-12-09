module Arkham.Asset.Assets.DissectionTools (dissectionTools, DissectionTools (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Token

newtype DissectionTools = DissectionTools AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dissectionTools :: AssetCard DissectionTools
dissectionTools = asset DissectionTools Cards.dissectionTools

instance HasModifiersFor DissectionTools where
  getModifiersFor (DissectionTools a) =
    controllerGets a
      $ [SkillModifier #agility 1 | a.use Resource >= 1]
      <> [SkillModifier #combat 1 | a.use Resource >= 2]
      <> [SanityModifier 1 | a.use Resource >= 3]

instance HasAbilities DissectionTools where
  getAbilities (DissectionTools a) =
    [ restrictedAbility a 1 ControlsThis
        $ freeReaction
        $ EnemyDefeated #after Anyone ByAny
        $ EnemyAt YourLocation
    ]

instance RunMessage DissectionTools where
  runMessage msg a@(DissectionTools attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Evidence 1
      pure a
    _ -> DissectionTools <$> liftRunMessage msg attrs
