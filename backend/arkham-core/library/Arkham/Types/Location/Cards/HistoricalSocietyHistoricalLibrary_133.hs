module Arkham.Types.Location.Cards.HistoricalSocietyHistoricalLibrary_133
  ( historicalSocietyHistoricalLibrary_133
  , HistoricalSocietyHistoricalLibrary_133(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype HistoricalSocietyHistoricalLibrary_133 = HistoricalSocietyHistoricalLibrary_133 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSocietyHistoricalLibrary_133
  :: LocationCard HistoricalSocietyHistoricalLibrary_133
historicalSocietyHistoricalLibrary_133 = locationWithRevealedSideConnections
  HistoricalSocietyHistoricalLibrary_133
  Cards.historicalSocietyHistoricalLibrary_133
  3
  (PerPlayer 2)
  NoSymbol
  [Circle]
  Triangle
  [Circle, Squiggle]

instance HasAbilities HistoricalSocietyHistoricalLibrary_133 where
  getAbilities (HistoricalSocietyHistoricalLibrary_133 attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility
            attrs
            1
            (CluesOnThis (AtLeast $ Static 1) <> CanDiscoverClues)
            (ReactionAbility
              (SkillTestResult
                Timing.After
                You
                (WhileInvestigating $ LocationWithId $ toId attrs)
                (SuccessResult AnyValue)
              )
              (HorrorCost (toSource attrs) YouTarget 2)
            )
          & abilityLimitL
          .~ PlayerLimit PerRound 1
        ]
      else
        [ mkAbility attrs 1 $ ForcedAbility $ EnemySpawns
            Timing.When
            (LocationWithId $ toId attrs)
            AnyEnemy
        ]

instance LocationRunner env => RunMessage env HistoricalSocietyHistoricalLibrary_133 where
  runMessage msg l@(HistoricalSocietyHistoricalLibrary_133 attrs) = case msg of
    UseCardAbility iid source _ 1 _
      | isSource attrs source && locationRevealed attrs -> l
      <$ push (DiscoverCluesAtLocation iid (toId attrs) 1 Nothing)
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (RevealLocation Nothing $ toId attrs)
    _ -> HistoricalSocietyHistoricalLibrary_133 <$> runMessage msg attrs
