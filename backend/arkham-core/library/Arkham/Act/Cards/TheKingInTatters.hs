module Arkham.Act.Cards.TheKingInTatters (
  TheKingInTatters (..),
  theKingInTatters,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype TheKingInTatters = TheKingInTatters ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theKingInTatters :: ActCard TheKingInTatters
theKingInTatters = act (3, A) TheKingInTatters Cards.theKingInTatters Nothing

instance HasAbilities TheKingInTatters where
  getAbilities (TheKingInTatters a) =
    [ restrictedAbility
        a
        1
        (OnLocation $ LocationWithoutClues <> LocationCanBeFlipped)
        $ FastAbility Free
    , mkAbility a 2
        $ ForcedAbility
        $ EnemyWouldBeDefeated Timing.When
        $ EnemyWithTitle "Hastur"
    ]

instance RunMessage TheKingInTatters where
  runMessage msg a@(TheKingInTatters attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      for_ mlid $ \lid -> do
        iids <- getInvestigatorIds
        noClues <- fieldP LocationClues (== 0) lid
        when noClues
          $ pushAll
          $ map (InvestigatorDiscardAllClues (toAbilitySource attrs 1)) iids
          <> [Flip iid source (LocationTarget lid)]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      whenM (selectAny $ enemyIs Enemies.hasturTheTatteredKing)
        $ push
        $ scenarioResolution 1
      whenM (selectAny $ enemyIs Enemies.hasturTheKingInYellow)
        $ push
        $ scenarioResolution 2
      whenM (selectAny $ enemyIs Enemies.hasturLordOfCarcosa)
        $ push
        $ scenarioResolution 3
      pure a
    _ -> TheKingInTatters <$> runMessage msg attrs
