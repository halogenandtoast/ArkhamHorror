module Arkham.Location.Cards.IlekVad (ilekVad, IlekVad (..)) where

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.History
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story
import Arkham.Trait (Trait (Relic, Ritual, Spell))

newtype IlekVad = IlekVad LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ilekVad :: LocationCard IlekVad
ilekVad = location IlekVad Cards.ilekVad 2 (PerPlayer 1)

instance HasModifiersFor IlekVad where
  getModifiersFor (InvestigatorTarget iid) (IlekVad attrs) = do
    here <- iid <=~> investigatorAt (toId attrs)
    playedCards <- historyPlayedCards <$> getHistory RoundHistory iid
    let cardMatcher = oneOf (map CardWithTrait [Spell, Ritual, Relic])
    let active = none (`cardMatch` cardMatcher) playedCards
    pure $ toModifiers attrs [ReduceCostOf cardMatcher 1 | here && active]
  getModifiersFor _ _ = pure []

instance HasAbilities IlekVad where
  getAbilities (IlekVad attrs) =
    veiled attrs []

instance RunMessage IlekVad where
  runMessage msg (IlekVad attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.thePalaceOfRainbows
      pure . IlekVad $ attrs & canBeFlippedL .~ False
    _ -> IlekVad <$> runMessage msg attrs
