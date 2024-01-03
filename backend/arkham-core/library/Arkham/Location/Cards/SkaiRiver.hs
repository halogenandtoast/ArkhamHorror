module Arkham.Location.Cards.SkaiRiver (skaiRiver, SkaiRiver (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story
import Arkham.Window (getBatchId)

newtype SkaiRiver = SkaiRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skaiRiver :: LocationCard SkaiRiver
skaiRiver = locationWith SkaiRiver Cards.skaiRiver 2 (Static 0) (canBeFlippedL .~ True)

instance HasAbilities SkaiRiver where
  getAbilities (SkaiRiver x) =
    withRevealedAbilities
      x
      [ restrictedAbility x 1 (exists $ LocationWithId (toId x) <> LocationCanBeFlipped)
          $ forced
          $ Leaves #when You
          $ be x
      ]

instance RunMessage SkaiRiver where
  runMessage msg l@(SkaiRiver attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getBatchId -> batchId) _ -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest iid (toAbilitySource attrs 1) (BatchTarget batchId) sType 2]
          | sType <- [#willpower, #agility]
          ]
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mTarget <- getSkillTestTarget
      case mTarget of
        Just (BatchTarget batchId) -> do
          -- the story should "technically" cancel the batch, but it is easier to do here
          pushAll [CancelBatch batchId, Flip iid (attrs.ability 1) (toTarget attrs)]
        _ -> error "Invalid target"
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.dreamlikeHorrors
      pure . SkaiRiver $ attrs & canBeFlippedL .~ False
    _ -> SkaiRiver <$> runMessage msg attrs
