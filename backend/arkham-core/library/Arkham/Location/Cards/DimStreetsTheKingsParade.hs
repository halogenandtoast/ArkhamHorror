module Arkham.Location.Cards.DimStreetsTheKingsParade (
  dimStreetsTheKingsParade,
  DimStreetsTheKingsParade (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype DimStreetsTheKingsParade = DimStreetsTheKingsParade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dimStreetsTheKingsParade :: LocationCard DimStreetsTheKingsParade
dimStreetsTheKingsParade =
  locationWith
    DimStreetsTheKingsParade
    Cards.dimStreetsTheKingsParade
    2
    (PerPlayer 1)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities DimStreetsTheKingsParade where
  getAbilities (DimStreetsTheKingsParade a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ DiscoveringLastClue
            Timing.After
            You
            (LocationWithId $ toId a)
      ]

instance RunMessage DimStreetsTheKingsParade where
  runMessage msg l@(DimStreetsTheKingsParade attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ LoseActions iid source 1
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theKingsParade
      pure . DimStreetsTheKingsParade $ attrs & canBeFlippedL .~ False
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          hastur <- selectJust $ EnemyWithTitle "Hastur"
          investigatorIds <- selectList $ investigatorEngagedWith hastur
          pushAll
            $ Exhaust (EnemyTarget hastur)
            : [DisengageEnemy iid' hastur | iid' <- investigatorIds]
          pure l
    _ -> DimStreetsTheKingsParade <$> runMessage msg attrs
