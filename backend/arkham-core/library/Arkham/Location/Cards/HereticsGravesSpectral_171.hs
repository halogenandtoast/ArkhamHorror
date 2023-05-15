module Arkham.Location.Cards.HereticsGravesSpectral_171 (
  hereticsGravesSpectral_171,
  hereticsGravesSpectral_171Effect,
  HereticsGravesSpectral_171 (..),
)
where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.SkillType
import Control.Monad.Trans.Maybe

newtype HereticsGravesSpectral_171 = HereticsGravesSpectral_171 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_171 :: LocationCard HereticsGravesSpectral_171
hereticsGravesSpectral_171 = location HereticsGravesSpectral_171 Cards.hereticsGravesSpectral_171 7 (Static 0)

instance HasModifiersFor HereticsGravesSpectral_171 where
  getModifiersFor (InvestigatorTarget iid) (HereticsGravesSpectral_171 a) = do
    mModifiers <- runMaybeT $ do
      guardM $ isTarget a <$> MaybeT getSkillTestTarget
      guardM $ (== Action.Investigate) <$> MaybeT getSkillTestAction
      guardM $ (== iid) <$> MaybeT getSkillTestInvestigator
      willpower <- lift $ getSkillValue SkillWillpower iid
      pure $ AnySkillValue willpower
    pure $ toModifiers a $ maybeToList mModifiers
  getModifiersFor _ _ = pure []

instance HasAbilities HereticsGravesSpectral_171 where
  getAbilities (HereticsGravesSpectral_171 a) =
    withRevealedAbilities
      a
      [haunted "Reduce your base {willpower} to 1 until the end of your next turn." a 1]

instance RunMessage HereticsGravesSpectral_171 where
  runMessage msg l@(HereticsGravesSpectral_171 attrs) = case msg of
    Flip _ _ target | isTarget attrs target -> do
      regular <- genCard Locations.hereticsGraves_171
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> HereticsGravesSpectral_171 <$> runMessage msg attrs

newtype Metadata = Metadata {inNextTurn :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HereticsGravesSpectral_171Effect = HereticsGravesSpectral_171Effect (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_171Effect :: EffectArgs -> HereticsGravesSpectral_171Effect
hereticsGravesSpectral_171Effect =
  cardEffect
    (HereticsGravesSpectral_171Effect . (`with` Metadata False))
    Cards.hereticsGravesSpectral_171

instance HasModifiersFor HereticsGravesSpectral_171Effect where
  getModifiersFor target (HereticsGravesSpectral_171Effect (a `With` _)) = do
    pure $ toModifiers a [BaseSkillOf SkillWillpower 1 | target == effectTarget a]

instance RunMessage HereticsGravesSpectral_171Effect where
  runMessage msg e@(HereticsGravesSpectral_171Effect (attrs `With` meta)) = case msg of
    BeginTurn iid | InvestigatorTarget iid == effectTarget attrs -> do
      pure . HereticsGravesSpectral_171Effect $ attrs `with` Metadata True
    EndTurn iid
      | inNextTurn meta
      , InvestigatorTarget iid == effectTarget attrs -> do
          push $ DisableEffect (toId attrs)
          pure e
    _ -> HereticsGravesSpectral_171Effect . (`with` meta) <$> runMessage msg attrs
