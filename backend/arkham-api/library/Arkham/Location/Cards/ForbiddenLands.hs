module Arkham.Location.Cards.ForbiddenLands (forbiddenLands) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message (pushWhen)
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (metaL)
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype Meta = Meta {skillTestCount :: Int}
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

newtype ForbiddenLands = ForbiddenLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenLands :: LocationCard ForbiddenLands
forbiddenLands =
  locationWith ForbiddenLands Cards.forbiddenLands 6 (Static 0)
    $ (canBeFlippedL .~ True)
    . (metaL .~ toJSON (Meta 0))

instance HasAbilities ForbiddenLands where
  getAbilities (ForbiddenLands attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ restricted attrs 1 (Here <> exists (be attrs <> LocationCanBeFlipped)) actionAbility

instance RunMessage ForbiddenLands where
  runMessage msg l@(ForbiddenLands attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #combat (Fixed 1)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      case skillTestCount (toResult attrs.meta) of
        0 -> do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 1)
        1 -> do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 2)
        2 -> do
          canFlip <- toId attrs <=~> LocationCanBeFlipped
          pushWhen canFlip $ Flip iid (attrs.ability 1) (toTarget attrs)
          pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 0)
        _ -> error "invalid count"
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure $ ForbiddenLands $ attrs & metaL .~ toJSON (Meta 0)
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.aShrineToTheGods
      pure . ForbiddenLands $ attrs & canBeFlippedL .~ False
    _ -> ForbiddenLands <$> liftRunMessage msg attrs
