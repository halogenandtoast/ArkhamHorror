module Arkham.Location.Cards.RuinsOfIb (ruinsOfIb, RuinsOfIb (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype RuinsOfIb = RuinsOfIb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfIb :: LocationCard RuinsOfIb
ruinsOfIb = locationWith RuinsOfIb Cards.ruinsOfIb 1 (PerPlayer 1) (canBeFlippedL .~ True)

instance HasAbilities RuinsOfIb where
  getAbilities (RuinsOfIb attrs) =
    veiled
      attrs
      [ restrictedAbility
          attrs
          1
          ( DuringSkillTest (WhileInvestigating $ LocationWithId $ toId attrs)
              <> exists (AssetControlledBy You <> DiscardableAsset)
          )
          $ ForcedAbility
          $ RevealChaosToken #after You
          $ oneOf [#skull, #cultist]
      ]

instance RunMessage RuinsOfIb where
  runMessage msg l@(RuinsOfIb attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- selectList $ assetControlledBy iid <> DiscardableAsset
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel asset [toDiscardBy iid (attrs.ability 1) asset] | asset <- assets]
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.ghostsOfTheDead
      pure . RuinsOfIb $ attrs & canBeFlippedL .~ False
    _ -> RuinsOfIb <$> runMessage msg attrs
