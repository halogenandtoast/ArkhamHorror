module Arkham.Homebrew.DarkMatter.Assets.MachineLearningAlgorithm (machineLearningAlgorithm) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (SkillTestEnded)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (skillTestSkillTypes)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype MachineLearningAlgorithm = MachineLearningAlgorithm AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machineLearningAlgorithm :: AssetCard MachineLearningAlgorithm
machineLearningAlgorithm = asset MachineLearningAlgorithm Cards.machineLearningAlgorithm

{- | "[reaction] After an investigator at your location performs a skill test,
exhaust Machine Learning Algorithm: That investigator gets +1 to all skill tests
of the same type until the end of the round."
-}
instance HasAbilities MachineLearningAlgorithm where
  getAbilities (MachineLearningAlgorithm a) =
    [ controlled_ a 1
        $ triggered (SkillTestEnded #after (colocatedWithMatch You) AnySkillTest) (exhaust a)
    ]

getTest :: HasCallStack => [Window] -> SkillTest
getTest [] = error "no skill test"
getTest ((windowType -> Window.SkillTestEnded st) : _) = st
getTest (_ : ws) = getTest ws

instance RunMessage MachineLearningAlgorithm where
  runMessage msg a@(MachineLearningAlgorithm attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getTest -> st) _ -> do
      for_ (skillTestSkillTypes st) \sType ->
        roundModifier (attrs.ability 1) st.investigator (SkillModifier sType 1)
      pure a
    _ -> MachineLearningAlgorithm <$> liftRunMessage msg attrs
