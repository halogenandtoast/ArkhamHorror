module Arkham.Location.Cards.DepthsOfDemheStepsOfThePalace
  ( depthsOfDemheStepsOfThePalace
  , DepthsOfDemheStepsOfThePalace(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Source
import Arkham.Story.Cards qualified as Story
import Arkham.Target

newtype DepthsOfDemheStepsOfThePalace = DepthsOfDemheStepsOfThePalace LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

depthsOfDemheStepsOfThePalace :: LocationCard DepthsOfDemheStepsOfThePalace
depthsOfDemheStepsOfThePalace = locationWith
  DepthsOfDemheStepsOfThePalace
  Cards.depthsOfDemheStepsOfThePalace
  4
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasModifiersFor DepthsOfDemheStepsOfThePalace where
  getModifiersFor (InvestigatorTarget iid) (DepthsOfDemheStepsOfThePalace a)
    | iid `on` a = pure $ toModifiers a [CannotPlay FastCard]
  getModifiersFor _ _ = pure []

instance RunMessage DepthsOfDemheStepsOfThePalace where
  runMessage msg l@(DepthsOfDemheStepsOfThePalace attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.stepsOfThePalace
      pure . DepthsOfDemheStepsOfThePalace $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.stepsOfThePalace -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      investigatorIds <- selectList $ investigatorEngagedWith hastur
      n <- getPlayerCountValue (PerPlayer 1)
      pushAll
        $ [ EnemyDamage hastur $ storyDamage (InvestigatorSource iid) n
          , Exhaust (EnemyTarget hastur)
          ]
        <> [ DisengageEnemy iid' hastur | iid' <- investigatorIds ]
      pure l
    _ -> DepthsOfDemheStepsOfThePalace <$> runMessage msg attrs
