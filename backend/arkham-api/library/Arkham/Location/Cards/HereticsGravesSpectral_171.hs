module Arkham.Location.Cards.HereticsGravesSpectral_171 (
  hereticsGravesSpectral_171,
  hereticsGravesSpectral_171Effect,
  HereticsGravesSpectral_171 (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isInvestigating)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Message (ReplaceStrategy (..))

newtype HereticsGravesSpectral_171 = HereticsGravesSpectral_171 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hereticsGravesSpectral_171 :: LocationCard HereticsGravesSpectral_171
hereticsGravesSpectral_171 = location HereticsGravesSpectral_171 Cards.hereticsGravesSpectral_171 7 (Static 0)

instance HasModifiersFor HereticsGravesSpectral_171 where
  getModifiersFor (HereticsGravesSpectral_171 a) =
    getSkillTestInvestigator >>= \case
      Nothing -> pure mempty
      Just iid -> maybeModified_ a iid do
        liftGuardM $ isInvestigating iid a.id
        willpower <- lift $ getSkillValue #willpower iid
        pure [AnySkillValue willpower]

instance HasAbilities HereticsGravesSpectral_171 where
  getAbilities (HereticsGravesSpectral_171 a) =
    extendRevealed1 a $ haunted "Reduce your base {willpower} to 1 until the end of your next turn." a 1

instance RunMessage HereticsGravesSpectral_171 where
  runMessage msg l@(HereticsGravesSpectral_171 attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      regular <- genCard Locations.hereticsGraves_171
      push $ ReplaceLocation (toId attrs) regular Swap
      pure l
    _ -> HereticsGravesSpectral_171 <$> liftRunMessage msg attrs

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
  getModifiersFor (HereticsGravesSpectral_171Effect (a `With` _)) = do
    modified_ a a.target [BaseSkillOf #willpower 1]

instance RunMessage HereticsGravesSpectral_171Effect where
  runMessage msg e@(HereticsGravesSpectral_171Effect (attrs `With` meta)) = runQueueT $ case msg of
    BeginTurn iid | isTarget iid attrs.target -> do
      pure . HereticsGravesSpectral_171Effect $ attrs `with` Metadata True
    EndTurn iid
      | inNextTurn meta
      , isTarget iid attrs.target -> do
          disableReturn e
    _ -> HereticsGravesSpectral_171Effect . (`with` meta) <$> liftRunMessage msg attrs
