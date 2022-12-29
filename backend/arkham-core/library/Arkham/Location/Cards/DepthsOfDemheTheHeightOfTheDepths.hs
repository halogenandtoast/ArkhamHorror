module Arkham.Location.Cards.DepthsOfDemheTheHeightOfTheDepths
  ( depthsOfDemheTheHeightOfTheDepths
  , DepthsOfDemheTheHeightOfTheDepths(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Damage
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Message
import Arkham.Scenarios.DimCarcosa.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Target

newtype DepthsOfDemheTheHeightOfTheDepths = DepthsOfDemheTheHeightOfTheDepths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

instance HasModifiersFor DepthsOfDemheTheHeightOfTheDepths where
  getModifiersFor (InvestigatorTarget iid) (DepthsOfDemheTheHeightOfTheDepths a)
    | iid `on` a = pure $ toModifiers a [CannotPlay FastCard]
  getModifiersFor _ _ = pure []

depthsOfDemheTheHeightOfTheDepths
  :: LocationCard DepthsOfDemheTheHeightOfTheDepths
depthsOfDemheTheHeightOfTheDepths = locationWith
  DepthsOfDemheTheHeightOfTheDepths
  Cards.depthsOfDemheTheHeightOfTheDepths
  4
  (PerPlayer 1)
  ((canBeFlippedL .~ True) . (revealedL .~ True))

instance RunMessage DepthsOfDemheTheHeightOfTheDepths where
  runMessage msg l@(DepthsOfDemheTheHeightOfTheDepths attrs) = case msg of
    Flip iid _ target | isTarget attrs target -> do
      readStory iid (toId attrs) Story.theHeightOfTheDepths
      pure . DepthsOfDemheTheHeightOfTheDepths $ attrs & canBeFlippedL .~ False
    ResolveStory _ story' | story' == Story.theHeightOfTheDepths -> do
      targets <- selectListMap InvestigatorTarget
        $ HealableInvestigator (toSource attrs) HorrorType Anyone
      setAsideDepthsOfDemhe <- getSetAsideCardsMatching
        $ CardWithTitle "Depths of Demhe"
      otherDepthsOfDemhe <- case setAsideDepthsOfDemhe of
        [] -> error "missing"
        (x : xs) -> sample (x :| xs)
      pushAll
        $ [ HealHorror target (toSource attrs) 5 | target <- targets ]
        <> [ReplaceLocation (toId attrs) otherDepthsOfDemhe]
      pure l
    _ -> DepthsOfDemheTheHeightOfTheDepths <$> runMessage msg attrs
