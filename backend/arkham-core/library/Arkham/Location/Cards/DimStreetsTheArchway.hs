module Arkham.Location.Cards.DimStreetsTheArchway (
  dimStreetsTheArchway,
  DimStreetsTheArchway (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype DimStreetsTheArchway = DimStreetsTheArchway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

dimStreetsTheArchway :: LocationCard DimStreetsTheArchway
dimStreetsTheArchway =
  locationWith
    DimStreetsTheArchway
    Cards.dimStreetsTheArchway
    2
    (PerPlayer 1)
    ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities DimStreetsTheArchway where
  getAbilities (DimStreetsTheArchway a) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ ForcedAbility
          $ DiscoveringLastClue
            Timing.After
            You
            (LocationWithId $ toId a)
      ]

instance RunMessage DimStreetsTheArchway where
  runMessage msg l@(DimStreetsTheArchway attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ LoseActions iid source 1
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theArchway
      pure . DimStreetsTheArchway $ attrs & canBeFlippedL .~ False
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source -> do
          mHealHorror <- getHealHorrorMessage attrs n iid
          for_ mHealHorror push
          pure l
    _ -> DimStreetsTheArchway <$> runMessage msg attrs
