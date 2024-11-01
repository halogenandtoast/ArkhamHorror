module Arkham.Location.Cards.RuinsOfIb (ruinsOfIb, RuinsOfIb (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Story

newtype RuinsOfIb = RuinsOfIb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfIb :: LocationCard RuinsOfIb
ruinsOfIb = location RuinsOfIb Cards.ruinsOfIb 1 (PerPlayer 1)

instance HasAbilities RuinsOfIb where
  getAbilities (RuinsOfIb attrs) =
    veiled
      attrs
      [ groupLimit PerTestOrAbility
          $ restricted
            attrs
            1
            ( DuringSkillTest (WhileInvestigating $ be attrs)
                <> exists (AssetControlledBy You <> DiscardableAsset)
            )
          $ forced
          $ RevealChaosToken #after You
          $ oneOf [#skull, #cultist]
      ]

instance RunMessage RuinsOfIb where
  runMessage msg l@(RuinsOfIb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      chooseTargetM iid assets $ toDiscardBy iid (attrs.ability 1)
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.ghostsOfTheDead
      pure . RuinsOfIb $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfIb <$> liftRunMessage msg attrs
