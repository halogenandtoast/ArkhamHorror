module Arkham.Location.Cards.ForbiddenLands (forbiddenLands, ForbiddenLands (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype Meta = Meta {skillTestCount :: Int}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

newtype ForbiddenLands = ForbiddenLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenLands :: LocationCard ForbiddenLands
forbiddenLands =
  locationWith
    ForbiddenLands
    Cards.forbiddenLands
    6
    (Static 0)
    $ (canBeFlippedL .~ True)
    . (metaL .~ toJSON (Meta 0))

instance HasAbilities ForbiddenLands where
  getAbilities (ForbiddenLands attrs) =
    withRevealedAbilities
      attrs
      [ skillTestAbility
          $ restrictedAbility
            attrs
            1
            (Here <> exists (LocationWithId (toId attrs) <> LocationCanBeFlipped))
            actionAbility
      ]

instance RunMessage ForbiddenLands where
  runMessage msg l@(ForbiddenLands attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 1)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      case skillTestCount (toResult attrs.meta) of
        0 -> do
          sid <- getRandom
          push $ beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 1)
        1 -> do
          sid <- getRandom
          push $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 2)
        2 -> do
          canFlip <- toId attrs <=~> LocationCanBeFlipped
          pushWhen canFlip $ Flip iid (attrs.ability 1) (toTarget attrs)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 0)
        _ -> error "invalid count"
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ assignDamage iid (attrs.ability 1) 1
      pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 0)
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.aShrineToTheGods
      pure . ForbiddenLands $ attrs & canBeFlippedL .~ False
    _ -> ForbiddenLands <$> runMessage msg attrs
