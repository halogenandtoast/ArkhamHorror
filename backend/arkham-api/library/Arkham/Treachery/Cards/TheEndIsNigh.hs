module Arkham.Treachery.Cards.TheEndIsNigh (theEndIsNigh, TheEndIsNigh (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheEndIsNigh = TheEndIsNigh TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndIsNigh :: TreacheryCard TheEndIsNigh
theEndIsNigh = treachery TheEndIsNigh Cards.theEndIsNigh

instance RunMessage TheEndIsNigh where
  runMessage msg t@(TheEndIsNigh attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push
        $ revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 1, CurrentAgendaStepCalculation (Fixed 4)]
      pure t
    FailedThisSkillTest _ (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      cultists <- select $ EnemyWithTrait Cultist
      if null cultists
        then push $ PlaceDoom (toSource attrs) (toTarget azathoth) 1
        else do
          doom <- getSum <$> foldMapM (fieldMap EnemyDoom Sum) cultists
          pushAll
            $ map (RemoveAllDoom (toSource attrs) . toTarget) cultists
            <> [PlaceDoom (toSource attrs) (toTarget azathoth) doom]
      pure t
    _ -> TheEndIsNigh <$> runMessage msg attrs
