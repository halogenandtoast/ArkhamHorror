module Arkham.Location.Cards.DimStreetsMappingTheStreets
  ( dimStreetsMappingTheStreets
  , DimStreetsMappingTheStreets(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.SkillType
import Arkham.Source
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype DimStreetsMappingTheStreets = DimStreetsMappingTheStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dimStreetsMappingTheStreets :: LocationCard DimStreetsMappingTheStreets
dimStreetsMappingTheStreets = locationWith
  DimStreetsMappingTheStreets
  Cards.dimStreetsMappingTheStreets
  2
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities DimStreetsMappingTheStreets where
  getAbilities (DimStreetsMappingTheStreets a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
        Timing.After
        You
        (LocationWithId $ toId a)
    ]

instance RunMessage DimStreetsMappingTheStreets where
  runMessage msg l@(DimStreetsMappingTheStreets attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ LoseActions iid source 1
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.mappingTheStreets
      pure . DimStreetsMappingTheStreets $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.mappingTheStreets -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- getPlayerCountValue (PerPlayer 1)
      pushAll
        [ beginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          SkillIntellect
          3
        , EnemyDamage hastur $ storyDamage (InvestigatorSource iid) n
        ]
      pure l
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        push $ InvestigatorAssignDamage iid source DamageAny 0 n
        pure l
    _ -> DimStreetsMappingTheStreets <$> runMessage msg attrs
