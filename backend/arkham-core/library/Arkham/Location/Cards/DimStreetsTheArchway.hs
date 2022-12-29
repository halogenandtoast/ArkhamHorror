module Arkham.Location.Cards.DimStreetsTheArchway
  ( dimStreetsTheArchway
  , DimStreetsTheArchway(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.SkillType
import Arkham.Story.Cards qualified as Story
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype DimStreetsTheArchway = DimStreetsTheArchway LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsTheArchway :: LocationCard DimStreetsTheArchway
dimStreetsTheArchway = locationWith
  DimStreetsTheArchway
  Cards.dimStreetsTheArchway
  2
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities DimStreetsTheArchway where
  getAbilities (DimStreetsTheArchway a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
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
    ResolveStory iid story' | story' == Story.theArchway -> do
      setAsideDimStreets <- getSetAsideCardsMatching
        $ CardWithTitle "Dim Streets"
      otherDimStreets <- case setAsideDimStreets of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      pushAll
        [ BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillIntellect
          3
        , ReplaceLocation (toId attrs) otherDimStreets
        ]
      pure l
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        mHealHorror <- getHealHorrorMessage attrs n iid
        for_ mHealHorror push
        pure l
    _ -> DimStreetsTheArchway <$> runMessage msg attrs
