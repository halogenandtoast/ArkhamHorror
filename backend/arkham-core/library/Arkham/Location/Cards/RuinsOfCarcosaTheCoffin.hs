module Arkham.Location.Cards.RuinsOfCarcosaTheCoffin
  ( ruinsOfCarcosaTheCoffin
  , RuinsOfCarcosaTheCoffin(..)
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
import Arkham.Source
import Arkham.Story.Cards qualified as Story
import Arkham.Timing qualified as Timing

newtype RuinsOfCarcosaTheCoffin = RuinsOfCarcosaTheCoffin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinsOfCarcosaTheCoffin :: LocationCard RuinsOfCarcosaTheCoffin
ruinsOfCarcosaTheCoffin = locationWith
  RuinsOfCarcosaTheCoffin
  Cards.ruinsOfCarcosaTheCoffin
  2
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance HasAbilities RuinsOfCarcosaTheCoffin where
  getAbilities (RuinsOfCarcosaTheCoffin a) = withBaseAbilities
    a
    [ mkAbility a 1 $ ForcedAbility $ DiscoveringLastClue
        Timing.After
        You
        (LocationWithId $ toId a)
    ]

instance RunMessage RuinsOfCarcosaTheCoffin where
  runMessage msg l@(RuinsOfCarcosaTheCoffin attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 1 0
      pure l
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theCoffin
      pure . RuinsOfCarcosaTheCoffin $ attrs & canBeFlippedL .~ False
    ResolveStory iid story' | story' == Story.theCoffin -> do
      hastur <- selectJust $ EnemyWithTitle "Hastur"
      n <- getPlayerCountValue (PerPlayer 1)
      push $ EnemyDamage hastur $ storyDamage (InvestigatorSource iid) n
      pure l
    _ -> RuinsOfCarcosaTheCoffin <$> runMessage msg attrs
